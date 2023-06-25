/* marsigood */

#include "vicmain_c"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"

#include "SimpleImage.h"

/* buffer sizes in main program */

#define MAX_FILES 5

////////////////////////////////////////////////////////////////////////


void main44()
{
    int i, j;
    int count, def;
    const size_t msgLen = 250;
    char msg[msgLen];

    int nids;
    char filename[PIG_MAX_FILENAME_SIZE+1];
    int nl = 0, ns = 0;
    int line, samp;
    int temp_nl, temp_ns;

    int inp_unit[MAX_FILES], mask_unit;
    int out_unit;
    int valid[MAX_FILES];

    // User Parameters

    int use_mask;
    int bands[MAX_FILES];
    double thresh[MAX_FILES];
    int mask_band;
    double mask_thresh;
    int good_value;
    int single_bad_value[MAX_FILES];
    int multi_bad_value;
    int mask_bad_value;

    // Images

    SimpleImage<double> inp[MAX_FILES];
    SimpleImage<double> mask;
    SimpleImage<unsigned char> out_img;

    PigFileModel *file_models[MAX_FILES+1];
    int num_file_models = 0;

    zvmessage("MARSIGOOD version 2020-07-06", "");

    zvpcnt("INP", &nids);

    if (nids < 1 || nids > MAX_FILES) {
	snprintf(msg, msgLen, "Invalid number of inputs, must be 1-%d", MAX_FILES);
	zvmessage(msg, "");
	zabend();
    }

    // Get user parameters

    // Goodness band in each input.  If less than needed, fill last one out.

    zvparm("BAND", bands, &count, &def, MAX_FILES, 0);
    if (count < nids) {
	zvmessage("More inputs than BANDS; using last BAND value to fill out","");
	for (i=count; i < nids; i++)
	    bands[i] = bands[count-1];
    }

    // Goodness threshold for each input.  Fill out if needed

    zvparmd("THRESH", thresh, &count, &def, MAX_FILES, 0);
    if (count < nids) {
	zvmessage("More inputs than THRESH values; using last THRESH value to fill out", "");
	for (i=count; i < nids; i++)
	    thresh[i] = thresh[count-1];
    }

    // Mask band
    zvp("MASK_BAND", &mask_band, &count);

    // Mask threshold
    zvparmd("MASK_THRESH", &mask_thresh, &count, &def, 1, 0);

    // Output goodness values
    zvp("GOOD_VALUE", &good_value, &count);
    zvp("MULTI_BAD", &multi_bad_value, &count);
    zvp("MASK_BAD", &mask_bad_value, &count);

    zvparm("SINGLE_BAD", single_bad_value, &count, &def, MAX_FILES, 0);
    if (count < nids) {
	zvmessage("More inputs than SINGLE_BAD values; using last SINGLE_BAD value to fill out","");
	for (i=count; i < nids; i++)
	    single_bad_value[i] = single_bad_value[count-1];
    }

    int first_valid = 0;
    for (i=0; i < nids; i++) {

	valid[i] = TRUE;

        zvpone("INP", filename, i+1, PIG_MAX_FILENAME_SIZE);

	if (strcmp(filename, "--") == 0) {
	    valid[i] = FALSE;		// No file for this slot
	    snprintf(msg, msgLen, "Skipping file %d by request", i+1);
	    zvmessage(msg, "");
	    if (first_valid == i)
		first_valid++;		// first file(s) invalid
	    continue;
	}
        zvunit(&inp_unit[i], "INP", i+1, NULL);
        zvopen(inp_unit[i], "op", "read", "open_act", "sa",
                "io_act", "sa", "u_format", "doub", NULL);

        zvget(inp_unit[i], "nl", &temp_nl, "ns", &temp_ns, NULL);
	if (i == first_valid) {
	    nl = temp_nl;
	    ns = temp_ns;
	}
	else if (temp_nl != nl || temp_ns != ns) {
	    snprintf(msg, msgLen, "Input %d is not the same size as input %d", i+1, first_valid+1);
	    zvmessage(msg, "");
	    zabend();
	}

	inp[i].alloc(nl, ns);

	// Read the data

	for (j=0; j < nl; j++) {
	    zvread(inp_unit[i], inp[i].linePtr(j),
		"LINE",j+1, "BAND",bands[i], NULL);
	}
	zvclose(inp_unit[i], NULL);

        PigMission *m = PigMission::getMissionObject(filename, &inp_unit[i]);
        if (m) {  //if we were able to create Mission object
            file_models[num_file_models++] = m->createFileModel(filename,
								inp_unit[i]);   
	}
    }

    if (first_valid == nids) {
	zvmessage("No valid inputs found; INP must have at least one non '--' file", "");
	zabend();
    }

    use_mask = 0;

    zvp("MASK", filename, &count);
    if (count != 0) {
	use_mask = 1;
        zvunit(&mask_unit, "MASK", 1, "u_name", filename, NULL);
        zvopen(mask_unit, "op", "read", "open_act", "sa",
                "io_act", "sa", "u_format", "doub", NULL);

	mask.alloc(nl, ns);

        zvget(mask_unit, "nl", &temp_nl, "ns", &temp_ns, NULL);
	if (temp_nl != nl || temp_ns != ns) {
	    zvmessage("Mask input is not the same size as input 1", "");
	    zabend();
	}

	mask.alloc(nl, ns);

	// Read the data

	for (j=0; j < nl; j++) {
	    zvread(mask_unit, mask.linePtr(j),
		"LINE",j+1, "BAND",mask_band, NULL);
	}
	zvclose(mask_unit, NULL);

        PigMission *m = PigMission::getMissionObject(filename, &mask_unit);
        if (m) {  //if we were able to create Mission object
            file_models[num_file_models++] = m->createFileModel(filename,
								mask_unit);   
	}
    }

    ////////////////////////////////////////////////////////////////////////
    // Do the work...

    // Allocate output image

    out_img.alloc(nl, ns);

    for (line = 0; line < nl; line++) {
	for (samp = 0; samp < ns; samp++) {
	    if (use_mask && mask.get(line,samp) < mask_thresh) {
		out_img.set(line, samp, mask_bad_value); // mask says no data
		continue;
	    }

	    int num_bad = 0;
	    int which_bad = 0;
	    int num_zero = 0;
	    int total_num = 0;

	    for (i=0; i < nids; i++) {
		if (!valid[i])
		    continue;		// skip this one
		total_num++;
		if (inp[i].get(line,samp) < thresh[i]) {	// it's bad
		    num_bad++;
		    which_bad = i;
		}
		if (inp[i].get(line,samp) == 0)
		    num_zero++;
	    }
	    if (num_zero == total_num) {	// all are zero!
		out_img.set(line, samp, 0);		// no data
	    } else if (num_bad == 0) {
		out_img.set(line, samp, good_value);	// yay!
	    } else if (num_bad == 1) {
		// say which was bad
		out_img.set(line, samp, single_bad_value[which_bad]);
	    } else {
		out_img.set(line, samp, multi_bad_value); // more than one bad
	    }
	}
    }

    ////////////////////////////////////////////////////////////////////////
    // Open output file.

    zvselpi(first_valid+1);	// set primary input to first real file

    zvunit(&out_unit, "OUT", 1, NULL);
    zvopen(out_unit, "op", "write",
               "u_ns", ns, "u_nl",nl, "u_nb", 1,
               "open_act", "sa", "u_org", "bsq",
               "u_format", "byte", "o_format", "byte", NULL);
    zvplabel(out_unit, 0, 1);

    // Write out the necessary label items.
    PigMission* m = PigMission::getMissionObject(filename, &inp_unit[0]);
    if (m) {  //if we were able to create Mission object
        PigLabelModel *labelModel = m->createLabelModel(out_unit);
	labelModel->setInstPlacement(file_models, num_file_models, 
                                     "GOODNESS_MAP", NULL, NULL);
    }
  
    ////////////////////////////////////////////////////////////////////////
    // Write out masked file...

    for (line = 0; line < nl; line++) {
        zvwrit(out_unit, out_img.linePtr(line),
                        "LINE", line+1, NULL);
    }

    zvclose(out_unit, NULL);

    zvmessage("Complete!", "");

}

