/////////////////////////////////////////////////////////////////////////////
//  marsinverter.cc
//////////////////////////////////////////////////////////////////////////////

#include "vicmain_c"

#include "mars_support.h"
#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"

#include "zvproto.h"
#include <stdio.h>

#define ILUT_SIZE	65536
#define ILUT_MIN	-32768
#define ILUT_MAX	32767

#define MAX_INPUTS 20
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS


int setupIlut(const char *ilut_name, short int ilut[ILUT_SIZE], PigMission *m,
		int *used_file, char *ilut_fn);

////////////////////////////////////////////////////////////////////////

void main44()
{
    int nids, nods, count; 
    int band = 1;
    int band_count = 1;
	const size_t msgLen = 1024;
    char msg[msgLen];

    char mission[64], instrument[64];
    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    int homogeneous_inputs = TRUE;
    PigSurfaceModel *surface = NULL;
    PigCoordSystem *dummy_cs;

    char *out_filenames[MAX_INPUTS];
    int in_unit, out_unit;
    int bits;

    short int *ibuf;
    short int ilut[ILUT_SIZE];

    // Get the input file list, and set everything up.  Most of these
    // models are unusd.

    mars_setup(nids, file_models, camera_in, pointing_in, surface, NULL, NULL,
		dummy_cs, mission, instrument, homogeneous_inputs,
		MAX_NL, MAX_NS, MAX_INPUTS);
   
    // Get the mission pointer

    PigMission *m = PigMission::getMissionObject(mission);

    // Get the output file list

    mars_get_filelist("OUT", nods, out_filenames, MAX_INPUTS, TRUE);
    if (nids != nods) {
	snprintf(msg,msgLen, "Number of inputs (%d) must match # of outputs (%d).",
			nids, nods);
	zvmessage(msg, "");
	zabend();
    }

    zvp("BAND", &band, &count);
    if (count == 0) {

        // No input band specified; process all bands
        band_count = file_models[0]->getNB();
        snprintf(msg, msgLen, "Number of bands to be processed is (%d)", band_count);
        zvmessage(msg, "");
    }
    else {
	
	// Inform user input band number is greater than number of bands in input image
	if (file_models[0]->getNB() < band){
	    snprintf(msg, msgLen, "Input band (%d) is greater than number of bands in input image. Band set to 1.", band);
	    zvmessage(msg, "");
	    band = 1;
	}
    }

    zvp("BITS", &bits, &count);
    if (count == 0) {			// defaulted
	bits = 12;
	if (file_models[0] != NULL) {
	    int b = file_models[0]->getOriginalSampleBits(0);
	    if (b != 0)
		bits = b;
	}
    }


    // Print out input status from labels

    mars_print_inputs(nids, pointing_in, camera_in, file_models,
			homogeneous_inputs, mission, instrument);

    // Loop over all the files

    for (int k = 0; k < nids; k++) {

	int nl = file_models[k]->getNL();
	int ns = file_models[k]->getNS();

	// Calculate the needed memory size for allocation

	ibuf = new short int[ns];
	if (ibuf == NULL) {
	    zvmessage("Unable to allocate input memory array!", "");
	    zabend();
	}

	// Open the input.  Note that it is read as short even if byte format

        in_unit = file_models[k]->getUnit();
        if (file_models[k]->isFileOpen())
	    file_models[k]->closeFile();

        zvopen(in_unit, "OP", "READ", "U_FORMAT", "HALF","OPEN_ACT", "SA", NULL);
        file_models[k]->setFileOpen(TRUE);

	// Initialize a no-op ILUT.  Fill all 16 bits just in case.

	for (int i=ILUT_MIN; i < ILUT_MAX; i++)
	    ilut[i-ILUT_MIN] = i;

	int inverted = FALSE;
	int used_file = FALSE;
	char ilut_fn[PIG_MAX_FILENAME_SIZE];		// for label
	if (file_models[k]->isIlutNeeded()) {
	    const char *ilut_name = file_models[k]->getIlutName();
	    if (ilut_name != NULL) {
		inverted = setupIlut(ilut_name, ilut, m, &used_file, ilut_fn);
	    }
	}

	// Open the output file

	zvselpiu(in_unit);
	zvunit(&out_unit, "ILUTTED", k, "U_NAME", out_filenames[k], NULL);
	int status = zvopen(out_unit, "OP", "WRITE", "U_FORMAT", "HALF", "OPEN_ACT", "SA",
		"O_FORMAT", "HALF", "U_NB", band_count, NULL);
	zvplabel(out_unit, 0, 1);

	// Set up labels.  Note that we set the derived image type to IMAGE
	// regardless.  If we're not doing any inverting, we still want to
	// update the source/input product ID's and the silly PDS keywords.

	PigLabelModel *lm = m->createLabelModel(out_unit);

	if (inverted) {
	    lm->setInverted(file_models, nids, TRUE, used_file, ilut_fn);
	    lm->setBitMask(bits);
	}
	else {
	    zvmessage("Operation is a no-op; image copied but unchanged except for ident labels","");
	    lm->writeProductIds(file_models, nids);
	    lm->setIdentification(NULL);
	}

	lm->setDerivedImageType(NULL);

	// Transfer the data over
	for (int j = 0; j < band_count; j++) {
	    for (int line=0; line < nl; line++) {
		zvread(in_unit, ibuf, "BAND",j+band , "LINE", line+1, NULL);
	        for (int samp=0; samp < ns; samp++) {
		    ibuf[samp] = ilut[ibuf[samp]-ILUT_MIN];
	        }
	        zvwrit(out_unit, ibuf, "BAND", j+1, "LINE", line+1, NULL);
	    } 
        }
		
	// Clean up

	delete ibuf;
	zvclose(out_unit, NULL);

	file_models[k]->closeFile();
    }   
}

////////////////////////////////////////////////////////////////////////
// Read the ILUT file and sets up the given ilut.  Returns TRUE if the file
// was read successfully, FALSE otherwise.  Note that FALSE does not necessarily
// mean error; it could mean there's no ILUT available.
//
// A few special ilut names are checked for here and an ilut for them is
// constructed on the fly.  
////////////////////////////////////////////////////////////////////////

int setupIlut(const char *ilut_name, short int ilut[ILUT_SIZE], PigMission *m,
		int *used_file, char *ilut_fn)
{
    char found_path[PIG_MAX_FILENAME_SIZE];
	const size_t msgLen = 1024;
    char msg[msgLen];

    *used_file = FALSE;

    // Check for the special names

    if (ilut_name == NULL || ilut_fn == NULL)
	return FALSE;			// nothing to do

    if (strcasecmp(ilut_name, "NONE") == 0)
	return FALSE;			// nothing to do

    // Bit shift.  These could be implemented by files just as easily.
    if (strncasecmp(ilut_name, "MSB_BIT", 7) == 0) {
	int bits = atoi(ilut_name+7);
	if (bits >= 7 && bits <= 15) {		// if not, try file
	    for (int i=0; i<256; i++) {
		ilut[i-ILUT_MIN] = i << (bits-7);
	    }
	    snprintf(msg, msgLen, "Shifting bits by %d", bits-7);
	    zvmessage(msg, "");
	    return TRUE;
	}
    }

    sprintf(ilut_fn, "ilut/%s_%s.txt", m->getHostID(), ilut_name);
    // Load the table from the file

    FILE *ilf = PigModelBase::openConfigFile(ilut_fn, found_path);

    if (ilf == NULL) {
	snprintf(msg, msgLen, "Warning: Unable to read ILUT file %s.  Ignored.",ilut_fn);
	zvmessage(msg, "");
	return FALSE;
    }

    char tmpLine[1024];
    while (fgets(tmpLine, 1024, ilf)) {
	int index=0;
	int value=0;
        char *pch = strtok(tmpLine, " ");
        if (pch != NULL) 
            index = atoi(pch);
        else {
	    snprintf(msg, msgLen, "Error reading a value from ILUT file %s", ilut_fn);
	    zvmessage(msg, "");
	}
        pch = strtok(NULL, " ");
        if (pch != NULL) 
            value = atoi(pch);
        else  {
	    snprintf(msg, msgLen, "Error reading a value2 from ILUT file %s", ilut_fn);
	    zvmessage(msg, "");
	}
	if (index < 0 || index >= ILUT_SIZE) {
	    snprintf(msg, msgLen, "Invalid index: %d read from ILUT file %s", index, ilut_fn);
	    zvmessage(msg, "");
	}
	else {
	    if (index > 255) {		// warn but accept it anyway
		snprintf(msg, msgLen, "Suspect index: %d read from ILUT file %s", index, ilut_fn);
		zvmessage(msg, "");
	    }
            ilut[index-ILUT_MIN] = value;
	}
    }
    snprintf(msg, msgLen, "Successfully read ILUT file %s", found_path);
    zvmessage(msg, "");
    fclose(ilf);
    *used_file = TRUE;
    return TRUE;
 }

