////////////////////////////////////////////////////////////////////////
// mars_edit_tiepoint.cc
//
// Edits a single tiepoint.  Returns 0 if okay, -1 if the user didn't change
// the tiepoint, or 1 if the user aborted (so the caller should exit the
// program).
////////////////////////////////////////////////////////////////////////

#include "mars_support.h"
#include "mars_tiepoints.h"

#include "PigFileModel.h"

#include "calltp.h"

#include "zvproto.h"

#include <stdio.h>
#include <string.h>

int mars_edit_tiepoint(int i, int &n_overlaps, TiePoint tiepoints[],
			PigFileModel *file_models[])
{
    int redo_it = TRUE;
    double **outArray;
    int num_temp;
    int return_status = 0;

    while (redo_it) {
	double temp_array[10][4];
	int user_status;
	redo_it = FALSE;
	temp_array[0][0] = tiepoints[i].left_sample;
	temp_array[0][1] = tiepoints[i].left_line;
	temp_array[0][2] = tiepoints[i].corrected_sample;
	temp_array[0][3] = tiepoints[i].corrected_line;
        num_temp = 1;

	file_models[tiepoints[i].left_image]->closeFile();
	file_models[tiepoints[i].right_image]->closeFile();
	outArray = display_points(
			file_models[tiepoints[i].left_image]->getFilename(),
			file_models[tiepoints[i].right_image]->getFilename(),
			temp_array, &num_temp, NULL, &user_status);

	if (user_status != 0) {

	    // remove rejected points

	    mars_remove_bad_points(tiepoints, n_overlaps, 0.0);
	    return 1;		// signal caller to save and exit the program
	}

	if (num_temp > 1) {
	    zvmessage("Require one tiepoint only", "");
	    redo_it = TRUE;
	}
    }		// while (redo_it)

    if (num_temp == 1) {

	if (tiepoints[i].left_sample == outArray[0][0] &&
	    tiepoints[i].left_line == outArray[0][1] &&
	    tiepoints[i].corrected_sample == outArray[0][2] &&
	    tiepoints[i].corrected_line == outArray[0][3])

	    return_status = -1;

	tiepoints[i].left_sample = outArray[0][0];
	tiepoints[i].left_line = outArray[0][1];
	tiepoints[i].corrected_sample = outArray[0][2];
	tiepoints[i].corrected_line = outArray[0][3];
	tiepoints[i].quality = 1.0;    // cuz the user said so (?)
	tiepoints[i].interactive = TRUE;
	tiepoints[i].active = TRUE;
	tiepoints[i].type = TIEPOINT_TRADITIONAL;
    }
    else {
	zvmessage("Tiepoint removed"," ");
	tiepoints[i].quality = 0.0;
	tiepoints[i].active = FALSE;
    }

    return return_status;
}

