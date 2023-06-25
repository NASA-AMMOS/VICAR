////////////////////////////////////////////////////////////////////////
// mars_edit_tiepoints.cc
//
// Edits a whole set of tiepoints.  Returns 0 if okay, or 1 if the user
// aborted with Special Exit Status (so the caller should exit the program).
//
// All tiepoints in the array must belong to the same image pair as
// specified in img1 and img2 (although they may be in either order).
// img1 and img2 are specified rather than getting it from the tiepoints
// in case there are 0 input tiepoints.
////////////////////////////////////////////////////////////////////////

#include "mars_support.h"
#include "mars_tiepoints.h"

#include "PigFileModel.h"

#include "calltp.h"

#include "zvproto.h"

#include <stdio.h>
#include <string.h>

int mars_edit_tiepoints(int &n_overlaps, int img1, int img2,
			TiePoint tiepoints[], PigFileModel *file_models[])
{
    double **outArray;
    int i;
    int return_status = 0;
    char msg[512];
    double temp_array[MARS_MAX_TIEPOINTS_PER_EDIT][4];
    int user_status;

    // Copy over to calltp format.  Swap the order if necessary.

    int num_temp = 0;

    for (i=0; i < n_overlaps; i++) {
	if (tiepoints[i].left_image == img1 &&
					tiepoints[i].right_image == img2) {
	    temp_array[num_temp][0] = tiepoints[i].left_sample;
	    temp_array[num_temp][1] = tiepoints[i].left_line;
	    temp_array[num_temp][2] = tiepoints[i].corrected_sample;
	    temp_array[num_temp][3] = tiepoints[i].corrected_line;
	    num_temp++;
	}
	else if (tiepoints[i].left_image == img2 &&
					tiepoints[i].right_image == img1) {
	    temp_array[num_temp][2] = tiepoints[i].left_sample;
	    temp_array[num_temp][3] = tiepoints[i].left_line;
	    temp_array[num_temp][0] = tiepoints[i].corrected_sample;
	    temp_array[num_temp][1] = tiepoints[i].corrected_line;
	    num_temp++;
	}
	else {
	    sprintf(msg, "INTERNAL ERROR: invalid image pair for tiepoint %d in mars_edit_tiepoints", i);
	    zvmessage(msg, "");
	}
    }

    file_models[img1]->closeFile();
    file_models[img2]->closeFile();

    outArray = display_points(
			file_models[img1]->getFilename(),
			file_models[img2]->getFilename(),
			temp_array, &num_temp, NULL, &user_status);


    // Copy results to output tiepoint array

    n_overlaps = 0;

    for (i=0; i < num_temp; i++) {
	tiepoints[n_overlaps].left_image = img1;
	tiepoints[n_overlaps].right_image = img2;
	tiepoints[n_overlaps].left_sample = outArray[i][0];
	tiepoints[n_overlaps].left_line = outArray[i][1];
	tiepoints[n_overlaps].right_sample = outArray[i][2];
	tiepoints[n_overlaps].right_line = outArray[i][3];
	tiepoints[n_overlaps].corrected_sample = outArray[i][2];
	tiepoints[n_overlaps].corrected_line = outArray[i][3];
	tiepoints[n_overlaps].quality = 1.0;    // cuz the user said so (?)
	tiepoints[n_overlaps].interactive = TRUE;
	tiepoints[n_overlaps].active = TRUE;
	tiepoints[n_overlaps].type = TIEPOINT_TRADITIONAL;
	n_overlaps++;
    }

    if (user_status != 0)	// special exit status...
	return 1;		// signal caller to save and exit the program

    return 0;
}

