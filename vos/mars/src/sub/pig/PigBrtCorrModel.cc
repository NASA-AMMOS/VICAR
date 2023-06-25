////////////////////////////////////////////////////////////////////////
// PigBrtCorrModel
//
// Base class for Brightness Correction models.  These are similar in
// effect to Radiometry models, but represent different kinds of corrections.
// BrtCorr models are intended to balance the radiometric seams between
// frames of a mosaic.  The fundamental difference, however, is that
// PigBrtCorrModel subclasses are based on the type of correction... not
// on the mission.  There really should be nothing particularly mission-
// specific about this model or its subclasses (although it is possible).
//
////////////////////////////////////////////////////////////////////////

#include <string.h>
#include "PigBrtCorrModel.h"
#include "PigBrtCorrLinear.h"
#include "PigBrtCorrHsiLin.h"
#include "PigMission.h"
#include "PigFileModel.h"
#include "PigXerces.h"

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigBrtCorrModel::PigBrtCorrModel(const char *mission,
			const char *instrument,
			int sl, int ss,
			int el, int es)

{
    _mission = _instrument = NULL;

    if (mission) _mission = strdup(mission);
    if (instrument) _instrument = strdup(instrument);

    _sl = sl;
    _ss = ss;
    _el = el;
    _es = es;
}

PigBrtCorrModel::PigBrtCorrModel(PigFileModel *file)
{
    _mission = strdup(file->getMissionName());
    _instrument = strdup(file->getInstrumentId());

    _sl = file->getFirstLine(0) - 1;		// convert to 0-based
    _ss = file->getFirstLineSample(0) - 1;

    if (_sl < 0) _sl = 0;	// clip if negative
    if (_ss < 0) _ss = 0;

    _el = _sl + file->getNL() - 1;	// compute ending line/sample
    _es = _ss + file->getNS() - 1;
}

PigBrtCorrModel::PigBrtCorrModel()
{
    _mission = _instrument = NULL;
    _sl = _ss = _el = _es = 0;
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigBrtCorrModel::~PigBrtCorrModel()
{
    if (_mission) 
        delete _mission;

    if (_instrument) 
        delete _instrument;

}

////////////////////////////////////////////////////////////////////////
// This is the method most users should call.  It will read the
// BRTCORR parameter to get the filename, read in the file, match
// entries to the given files, and create elements in the brt_models
// array as needed.  Note, this nulls out the brt_models array so
// there should be no active pointers in it.  Also, it is assumed the
// brt_models array is enough to store nids pointers.
//
// The caller's PDF should have:
//
// PARM BRTCORR TYPE=STRING COUNT=(0:1) DEFAULT=--
//
// The rad correction models are needed by some correction types (e.g.
// HsiLin).  It can be null if you don't need rad.
//
////////////////////////////////////////////////////////////////////////

void PigBrtCorrModel::createBrtCorrModels(int nids,
	PigFileModel *file_models[], PigBrtCorrModel *brt_models[],
	RadiometryModel *rad[])
{
    int i, count;
    char brt_file[PIG_MAX_FILENAME_SIZE+1];
    char msg[256];

    for (i=0; i < nids; i++)
	brt_models[i] = NULL;

    PigModelBase::getStaticParam("BRTCORR", brt_file, &count,
			1, PIG_MAX_FILENAME_SIZE);

    if (count == 0)
	return;			// Nothing to create

    PigXerces::initialize();

    DOMDocument *doc = PigXerces::parseFile(brt_file);
    if (doc == NULL) {
	PigXerces::close();
	sprintf(msg, "Error loading brightness correction file '%d'", brt_file);
	printStaticMsg(msg, PigMsgError);
	return;
    }

    DOMElement *root = doc->getDocumentElement();	// should be brightness_correction
    char *version = PigXerces::getAttribute(root, "version");
    if (strcmp(version, "1.0") != 0) {
	printStaticMsg("Version error reading brtcorr file!  Continuing as version 1.0", PigMsgError);
    }
    XMLString::release(&version);

    ////////!!!!
    // Note that we ignore origination here, since it is documentation only.
    // We also ignore priority.  We really should look at it, but at present
    // we are completely ignoring the solution_id so there's no point in it.
    ////////!!!!

    // Get all brt_solution's, process each in turn

    DOMNodeList *sols = PigXerces::getElementsByTagName(root, "brt_solution");

    int nomatch_count = 0;
    for (int solnum = 0; solnum < sols->getLength(); solnum++) {

	DOMElement *soln = PigXerces::nextElement(sols, solnum);

	DOMElement *image = PigXerces::getOneElement(soln, "image");
	if (image == NULL) {
	    printStaticMsg("brt_solution element without an image tag: ignored",
				PigMsgError);

	    continue;
	}

	// We ignore everything except unique_id because we're only doing
	// "tight" style matching

	char *id = PigXerces::getAttribute(image, "unique_id");

	if (id == NULL || strlen(id) == 0) {
	    printStaticMsg("Error: malformed unique_id attribute in <image> tag; ignored", PigMsgError);
	    XMLString::release(&id);
	    continue;
	}

	int slot = -1;

	// Find the slot in the brt_models array to put this into

	for (i = 0; i < nids; i++) {
            if(file_models[i] == NULL)
                continue;

	    char uid[256];
	    file_models[i]->getUniqueId(uid);
	    if (strcmp(uid, id) == 0) {		// found the matching place
		slot = i;
		break;
	    }

	    file_models[i]->getUniqueId2(uid);
	    if (strcmp(uid, id) == 0) {		// found the matching place
		slot = i;
		break;
	    }
	}
	if (slot == -1) {
	    if (nomatch_count == 0) {
	        sprintf(msg, "Unable to find matching image for entry '%s' in brt_corr file: ignored", id);
	        printStaticMsg(msg, PigMsgWarning);
	    }
	    if (nomatch_count == 1)
		printStaticMsg("Further no-match errors suppressed", PigMsgWarning);
	    nomatch_count++;
	    XMLString::release(&id);
	    continue;
	}
	XMLString::release(&id);

	// Got a slot, now create the structure and stuff it in

	DOMElement *correction = PigXerces::getOneElement(soln, "correction");
	if (correction == NULL) {
	    sprintf(msg,"brt_solution element without a correction tag (image #%d): ignored", slot);
	    printStaticMsg(msg, PigMsgError);
	    continue;
	}

	PigBrtCorrModel *mdl = createOneModel(file_models[slot], correction,
					(rad == NULL) ? NULL: rad[slot]);

	brt_models[slot] = mdl;

    }

    // Fill in empty slots with a null correction

    for (i=0; i < nids; i++) {
        if(file_models[i] == NULL)
            continue;
	if (brt_models[i] == NULL) {
	    brt_models[i] = new PigBrtCorrLinear(file_models[i]);
	}
    }

    doc->release();
    PigXerces::close();
}

////////////////////////////////////////////////////////////////////////
// Create a single BrtCorr model based on the type in the given XML node.
// Should only be called by createBrtCorrModels() but is public here just
// in case.  It should be given the <correction> element.
////////////////////////////////////////////////////////////////////////

PigBrtCorrModel *PigBrtCorrModel::createOneModel(PigFileModel *file_model,
		DOMElement *el, RadiometryModel *rad)
{
    PigBrtCorrModel *mdl = NULL;
    int status;
    char msg[256];
    char mytype[256];

    char *type = PigXerces::getAttribute(el, "type");
    strcpy(mytype, type);	// avoid having to release it everywhere
    XMLString::release(&type);

    if (strcasecmp(mytype, "LINEAR") == 0) {

	mdl = new PigBrtCorrLinear(file_model, el, &status);
	if (status != 0) {
	    delete mdl;
	    return NULL;
	}

	return mdl;
    }
    if (strcasecmp(mytype, "HSI_LIN") == 0) {

	mdl = new PigBrtCorrHsiLin(file_model, el, &status, rad);
	if (status != 0) {
	    delete mdl;
	    return NULL;
	}

	return mdl;
    }
    else {
	sprintf(msg, "Unrecognized correction type: '%s'; ignored", mytype);
	printStaticMsg(msg, PigMsgError);
    }

    return NULL;
}

////////////////////////////////////////////////////////////////////////
// Print the fields of this class.  Subclasses should normally call this,
// then add their own fields.
////////////////////////////////////////////////////////////////////////

void PigBrtCorrModel::print()

{
    char msg[256];

    sprintf(msg, "Mission: %s", _mission);
    printInfo(msg);
    sprintf(msg, "Instrument: %s", _instrument);
    printInfo(msg);
    sprintf(msg, "starting line: %d", _sl);
    printInfo(msg);
    sprintf(msg, "ending line: %d", _el);
    printInfo(msg);
    sprintf(msg, "starting sample: %d", _ss);
    printInfo(msg);
    sprintf(msg, "ending sample: %d", _es);
    printInfo(msg);
}

////////////////////////////////////////////////////////////////////////
// Apply the correction to an image.  max_nl and max_ns represent the
// physical size of the buffer, not the logical extent.  The _sl, _ss,
// _el, and _es member variables define where this physical buffer lies
// in the logical image.  So, an _sl of 2 means that the first line of
// the physical buffer corresponds to the second line of the full-frame
// image.
// Note: band is 0-based.
//
// Base class does nothing; this should be overridden by subclasses
////////////////////////////////////////////////////////////////////////

void PigBrtCorrModel::applyCorrectionInternal(void *image,
		int max_nl, int max_ns, int is_float, int band)
{
    return;

}

////////////////////////////////////////////////////////////////////////
// Write this piece of the model to the label
////////////////////////////////////////////////////////////////////////

int PigBrtCorrModel::writeToLabel(PigLabelModel *lbl, int index)
{
    return 0;
}

