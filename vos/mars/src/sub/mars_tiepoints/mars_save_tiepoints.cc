#include "mars_tiepoints.h"

#include "PigFileModel.h"
#include "PigCoordSystem.h"

#include <stdio.h>
#include <string.h>

////////////////////////////////////////////////////////////////////////
// WARNING
// Methods in this file are VERY similar to those in mars_save_overlaps.cc.
// Make sure to maintain in parallel!!!!
////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////
// OLD STYLE
//
// mars_save_tiepoints.cc
//
// Save the tiepoints to a file.  Returns 0 for success, 1 for file open
// error, and 2 for file write error.  Error messages are NOT printed.
// See mars_load_tiepoints for file format definition (only version 2 is
// supported).
////////////////////////////////////////////////////////////////////////

int mars_save_tiepoints(char *name, TiePoint tiepoints[], int n)
{
    FILE *fout;
    char msg[255];
    int i, status;

    if ((fout = fopen(name, "w")) == NULL) {
	return 1;			// Error opening file!
    }

    // write saved tiepoints

    fprintf(fout, "mars_tiepoint_list v2\n");		// header
    fprintf(fout, "%d\n", n);

    for (i = 0; i < n; i++) {
	status = fprintf(fout, "%d %d %f %f %f %f %f %f %f %d\n",
		tiepoints[i].left_image, tiepoints[i].right_image,
		tiepoints[i].left_sample, tiepoints[i].left_line,
		tiepoints[i].right_sample, tiepoints[i].right_line,
		tiepoints[i].corrected_sample, tiepoints[i].corrected_line,
		tiepoints[i].quality,
		tiepoints[i].interactive);
	if (status <= 0) {
	    fclose(fout);
	    return 2;			// Error writing file!
	}
    }
    fclose(fout);

    return 0;		// success
}


////////////////////////////////////////////////////////////////////////
// NEW STYLE
//
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
        PigCoordSystem *cs)     // what XYZ's/angles are in, may be null
{
    int i, count;
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

// Now set up the keys in the tiepoint list.  Conveniently, the key just
// happens to be equal to the image number plus start_key.
// No need to check for active or "right" being used by this tiepoint type,
// since it won't hurt to assign the key anyway.

    for (i = 0; i < n; i++) {
	tiepoints[i].left_key = tiepoints[i].left_image + start_key;
	tiepoints[i].right_key = tiepoints[i].right_image + start_key;
    }

    int status = mars_save_tiepoints(name, tiepoints, n,
				unique_ids, keys, n_files, cs);

    for (i = 0; i < n_files; i++) {
	delete unique_ids[i];
    }
    delete unique_ids;
    delete keys;

    return status;

}


////////////////////////////////////////////////////////////////////////
// NEW STYLE
//
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
        char *name,             // filename
        TiePoint tiepoints[],   // tiepoints to save
        int n,                  // # of tiepoints
        char *unique_ids[],     // unique ID's from files
        int keys[],             // specific keys per file (MAY NOT be null)
        int n_files,            // number of both the above
        PigCoordSystem *cs)     // what XYZ's/angles are in, may be null
{
    FILE *fout;
    char msg[255];
    int i, s;
 
#define CHK if (s<0) goto error;

    if ((fout = fopen(name, "w")) == NULL) {
	return -3;			// Error opening file!
    }

    // Write header

    s=fprintf(fout, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
    CHK
    s=fprintf(fout, "<tiepoint_file version=\"1.0\">\n");
    CHK
    s=fprintf(fout, "<tiepoint_set>\n");
    CHK

    // Write coord system

    if (cs != NULL) {
	s=fprintf(fout, "  <reference_frame name=\"%s\"",cs->getFrameShortName());
	CHK
	PigCSReference *ident = cs->getIdentity();
	for (i = 0; i < ident->getNumIndices(); i++) {
	    s=fprintf(fout, " index%d=\"%d\"", i+1, ident->getIndex(i));
	    CHK
	}
	s=fprintf(fout, "/>\n");
	CHK
    }

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

    // Write tiepoint list

    s=fprintf(fout, "  <tiepoints>\n");
    CHK

    for (i = 0; i < n; i++) {
	if (!tiepoints[i].active)
	    continue;			// skip

	switch (tiepoints[i].type) {

	    case TIEPOINT_TRADITIONAL:
	    case TIEPOINT_MISS_DISTANCE:
		s=fprintf(fout, "    <tie type=\"%d\" left_key=\"%d\" right_key=\"%d\">\n", 
			tiepoints[i].type, tiepoints[i].left_key, tiepoints[i].right_key);
		CHK
		s=fprintf(fout, "      <left      line=\"%f\" samp=\"%f\"/>\n",
			tiepoints[i].left_line, tiepoints[i].left_sample);
		CHK
		s=fprintf(fout, "      <projected line=\"%f\" samp=\"%f\"/>\n",
			tiepoints[i].right_line, tiepoints[i].right_sample);
		CHK
		s=fprintf(fout, "      <right     line=\"%f\" samp=\"%f\"/>\n",
			tiepoints[i].corrected_line, tiepoints[i].corrected_sample);
		CHK
		break;

	    case TIEPOINT_FIDUCIAL:
		s=fprintf(fout, "    <tie type=\"1\" left_key=\"%d\">\n", 
			tiepoints[i].left_key);
		CHK
		s=fprintf(fout, "      <left      line=\"%f\" samp=\"%f\"/>\n",
			tiepoints[i].left_line, tiepoints[i].left_sample);
		CHK
		s=fprintf(fout, "      <xyz x=\"%lf\" y=\"%lf\" z=\"%lf\"/>\n",
			tiepoints[i].xyz.getX(), tiepoints[i].xyz.getY(),
			tiepoints[i].xyz.getZ());
		CHK
		break;

	    case TIEPOINT_1D:
		s=fprintf(fout, "    <tie type=\"2\" left_key=\"%d\" right_key=\"%d\">\n", 
			tiepoints[i].left_key, tiepoints[i].right_key);
		CHK
		s=fprintf(fout, "      <left      line=\"%f\" samp=\"%f\"/>\n",
			tiepoints[i].left_line, tiepoints[i].left_sample);
		CHK
		s=fprintf(fout, "      <projected line=\"%f\" samp=\"%f\"/>\n",
			tiepoints[i].right_line, tiepoints[i].right_sample);
		CHK
		s=fprintf(fout, "      <right     line=\"%f\" samp=\"%f\"/>\n",
			tiepoints[i].corrected_line, tiepoints[i].corrected_sample);
		CHK
		s=fprintf(fout, "      <angle degrees=\"%lf\"/>\n",
			tiepoints[i].angle);
		CHK
		break;

	    case TIEPOINT_INFINITY:
		s=fprintf(fout, "    <tie type=\"3\" left_key=\"%d\" right_key=\"%d\">\n", 
			tiepoints[i].left_key, tiepoints[i].right_key);
		CHK
		s=fprintf(fout, "      <left      line=\"%f\" samp=\"%f\"/>\n",
			tiepoints[i].left_line, tiepoints[i].left_sample);
		CHK
		s=fprintf(fout, "      <projected line=\"%f\" samp=\"%f\"/>\n",
			tiepoints[i].right_line, tiepoints[i].right_sample);
		CHK
		s=fprintf(fout, "      <right     line=\"%f\" samp=\"%f\"/>\n",
			tiepoints[i].corrected_line, tiepoints[i].corrected_sample);
		CHK
		break;

	    case TIEPOINT_ORBITAL_XY:
		s=fprintf(fout, "    <tie type=\"4\" left_key=\"%d\">\n", 
			tiepoints[i].left_key);
		CHK
		s=fprintf(fout, "      <left      line=\"%f\" samp=\"%f\"/>\n",
			tiepoints[i].left_line, tiepoints[i].left_sample);
		CHK
		s=fprintf(fout, "      <xy x=\"%lf\" y=\"%lf\"/>\n",
			tiepoints[i].xyz.getX(), tiepoints[i].xyz.getY());
		CHK
		break;

	    case TIEPOINT_ORBITAL_XYZ:
		s=fprintf(fout, "    <tie type=\"5\" left_key=\"%d\">\n", 
			tiepoints[i].left_key);
		CHK
		s=fprintf(fout, "      <left      line=\"%f\" samp=\"%f\"/>\n",
			tiepoints[i].left_line, tiepoints[i].left_sample);
		CHK
		s=fprintf(fout, "      <xyz x=\"%lf\" y=\"%lf\" z=\"%lf\"/>\n",
			tiepoints[i].xyz.getX(), tiepoints[i].xyz.getY(),
			tiepoints[i].xyz.getZ());
		CHK
		break;

	    case TIEPOINT_AZIMUTH:
		s=fprintf(fout, "    <tie type=\"6\" left_key=\"%d\">\n", 
			tiepoints[i].left_key);
		CHK
		s=fprintf(fout, "      <left      line=\"%f\" samp=\"%f\"/>\n",
			tiepoints[i].left_line, tiepoints[i].left_sample);
		CHK
		s=fprintf(fout, "      <azimuth degrees=\"%lf\"/>\n",
			tiepoints[i].angle);
		CHK
		break;

	    case TIEPOINT_ELEVATION:
		s=fprintf(fout, "    <tie type=\"7\" left_key=\"%d\">\n", 
			tiepoints[i].left_key);
		CHK
		s=fprintf(fout, "      <left      line=\"%f\" samp=\"%f\"/>\n",
			tiepoints[i].left_line, tiepoints[i].left_sample);
		CHK
		s=fprintf(fout, "      <elevation degrees=\"%lf\"/>\n",
			tiepoints[i].angle);
		CHK
		break;

	    case TIEPOINT_Z_SURFACE:
		s=fprintf(fout, "    <tie type=\"8\" left_key=\"%d\" right_key=\"%d\">\n", 
			tiepoints[i].left_key, tiepoints[i].right_key);
		CHK
		s=fprintf(fout, "      <left      line=\"%f\" samp=\"%f\"/>\n",
			tiepoints[i].left_line, tiepoints[i].left_sample);
		CHK
		s=fprintf(fout, "      <projected line=\"%f\" samp=\"%f\"/>\n",
			tiepoints[i].right_line, tiepoints[i].right_sample);
		CHK
		s=fprintf(fout, "      <right     line=\"%f\" samp=\"%f\"/>\n",
			tiepoints[i].corrected_line, tiepoints[i].corrected_sample);
		CHK
		s=fprintf(fout, "      <z z=\"%lf\"/>\n",
			tiepoints[i].xyz.getZ());
		CHK
		break;

	    case TIEPOINT_DYNAMIC_XYZ:
		s=fprintf(fout, "    <tie type=\"9\" left_key=\"%d\" right_key=\"%d\">\n", 
			tiepoints[i].left_key, tiepoints[i].right_key);
		CHK
		s=fprintf(fout, "      <left      line=\"%f\" samp=\"%f\"/>\n",
			tiepoints[i].left_line, tiepoints[i].left_sample);
		CHK
		s=fprintf(fout, "      <projected line=\"%f\" samp=\"%f\"/>\n",
			tiepoints[i].right_line, tiepoints[i].right_sample);
		CHK
		s=fprintf(fout, "      <right     line=\"%f\" samp=\"%f\"/>\n",
			tiepoints[i].corrected_line, tiepoints[i].corrected_sample);
		CHK
		break;

	    default:
		char msg[256];
		sprintf(msg, "ERROR: Unknown tiepoint type %d encountered saving file.  Tiepoint ignored.", tiepoints[i].type);
		PigModelBase::printStaticMsg(msg, PigMsgError);
		continue;			// skip the trailer below
	}


        // Additional data (if active) about tiepoints residual before/after adjustment of navigation
        if (tiepoints[i].nav_residual) {
           s=fprintf(fout, "      <left_init_residual     line=\"%f\" samp=\"%f\"/>\n",
                     tiepoints[i].left_init_residual_line, tiepoints[i].left_init_residual_sample);
           CHK
           s=fprintf(fout, "      <left_final_residual    line=\"%f\" samp=\"%f\"/>\n",
                     tiepoints[i].left_final_residual_line, tiepoints[i].left_final_residual_sample);
           CHK
           s=fprintf(fout, "      <right_init_residual    line=\"%f\" samp=\"%f\"/>\n",
                     tiepoints[i].right_init_residual_line, tiepoints[i].right_init_residual_sample);
           CHK
           s=fprintf(fout, "      <right_final_residual   line=\"%f\" samp=\"%f\"/>\n",
                     tiepoints[i].right_final_residual_line, tiepoints[i].right_final_residual_sample);
           CHK
           s=fprintf(fout, "      <init_miss dist=\"%f\"/>\n",
                     tiepoints[i].init_miss_dist);
           CHK  
           s=fprintf(fout, "      <final_miss dist=\"%f\"/>\n",
                     tiepoints[i].final_miss_dist);
           CHK  
           s=fprintf(fout, "      <init_xyz x=\"%lf\" y=\"%lf\" z=\"%lf\"/>\n",
                     tiepoints[i].init_xyz.getX(), tiepoints[i].init_xyz.getY(),
                     tiepoints[i].init_xyz.getZ());
           CHK  
           s=fprintf(fout, "      <final_xyz x=\"%lf\" y=\"%lf\" z=\"%lf\"/>\n",
                     tiepoints[i].final_xyz.getX(), tiepoints[i].final_xyz.getY(),
                     tiepoints[i].final_xyz.getZ());
           CHK  
           s=fprintf(fout, "      <track  id=\"%d\"/>\n", tiepoints[i].track);

        }


	// Common trailer for all tiepoints

	s=fprintf(fout, "      <flags quality=\"%f\" interactive=\"%d\"/>\n",
		tiepoints[i].quality, tiepoints[i].interactive);
	CHK
	s=fprintf(fout, "    </tie>\n");
	CHK
    }

    // Clean up end of file

    s=fprintf(fout, "  </tiepoints>\n");
    CHK
    s=fprintf(fout, "</tiepoint_set>\n");
    CHK
    s=fprintf(fout, "</tiepoint_file>\n");
    CHK

    fclose(fout);

    return 0;		// success

error:
    fclose(fout);
    return -2;		// write error

}

