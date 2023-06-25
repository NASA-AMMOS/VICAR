////////////////////////////////////////////////////////////////////////
// PigBrtCorrLinear
//
// Implementation of "Linear" brightness correction model.  This simply
// applies a constant multiplicative and additive factor to the entire image
// (i.e. i' = i * a + b)
//
////////////////////////////////////////////////////////////////////////

#include <string.h>
#include "PigBrtCorrLinear.h"
#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"

////////////////////////////////////////////////////////////////////////
// Constructor.  Returns 0 in *status on success
////////////////////////////////////////////////////////////////////////

PigBrtCorrLinear::PigBrtCorrLinear() : PigBrtCorrModel()
{
    _add = 0.0;
    _mult = 1.0;
}

PigBrtCorrLinear::PigBrtCorrLinear(PigFileModel *file)
		: PigBrtCorrModel(file)
{
    _add = 0.0;
    _mult = 1.0;
}

PigBrtCorrLinear::PigBrtCorrLinear(PigFileModel *file, DOMElement *el,
				int *status)
		: PigBrtCorrModel(file)
{
    char msg[256];

    *status = -1;		// assume failure

    // Now read the parameters from the XML file

    _add = 0.0;
    _mult = 1.0;

    DOMNodeList *params = PigXerces::getElementsByTagName(el, "parameter");
    for (int i = 0; i < params->getLength(); i++) {

	DOMElement *param = PigXerces::nextElement(params, i);
	char *pid = PigXerces::getAttribute(param, "id");
	if (pid == NULL || strlen(pid) == 0) {
	    printStaticMsg("Malformed <param> tag in brt corr file, ignored",
					PigMsgError);
	    XMLString::release(&pid);
	    return;
	}

	if (strcasecmp(pid, "ADD") == 0) {
	    _add = PigXerces::getAttributeDouble(param, "value", 0.0);
	}
	else if (strcasecmp(pid, "MULT") == 0) {
	    _mult = PigXerces::getAttributeDouble(param, "value", 1.0);
	}
	else {
	    sprintf(msg, "Unknown parameter type '%s' in brt corr file, ignored", pid);
	    printStaticMsg(msg, PigMsgError);	// keep going
	}

	XMLString::release(&pid);
    }

    *status = 0;		// success
    return;

}

////////////////////////////////////////////////////////////////////////
// Print the fields of this class.  Subclasses should normally call this,
// then add their own fields.
////////////////////////////////////////////////////////////////////////

void PigBrtCorrLinear::print()

{
    char msg[256];

    PigBrtCorrModel::print();
    sprintf(msg, "Additive constant: %lf", _add);
    printInfo(msg);
    sprintf(msg, "Multiplicative constant: %lf", _mult);
    printInfo(msg);
}

////////////////////////////////////////////////////////////////////////
// Apply the correction to an image.  max_nl and max_ns represent the
// physical size of the buffer, not the logical extent.  The _sl, _ss,
// _el, and _es member variables define where this physical buffer lies
// in the logical image.  So, an _sl of 2 means that the first line of
// the physical buffer corresponds to the second line of the full-frame
// image.
// Note: band is 0-based
////////////////////////////////////////////////////////////////////////

// Assumes "image_*", and "max_ns" are local variables
#define IMAGE(line, samp) (*((image_int) + (line) * max_ns + (samp)))
#define IMAGEF(line, samp) (*((image_float) + (line) * max_ns + (samp)))

void PigBrtCorrLinear::applyCorrectionInternal(void *image,
		int max_nl, int max_ns, int is_float, int band)
{
    int i, j;
    char msg[256];

    sprintf(msg, "Applying correction of mult=%lf add=%lf", _mult, _add);
    printInfo(msg);

    short int *image_int = (short int *)image;
    float *image_float = (float *)image;

    for (i=0; i < (_el - _sl + 1); i++) {
	for (j=0; j < (_es - _ss + 1); j++) {

	    // All that infrastructure for these two lines of code...
	    // Make sure to preserve 0.
	    // Also for the non-float case, if input is non-0, make sure
	    // output is non-0 too... so it doesn't turn transparent.

	    if (is_float) {
		float dn = IMAGEF(i,j);
		if (dn != 0.0)
		    IMAGEF(i,j) = dn * _mult + _add;
	    }
	    else {
		short int dn = IMAGE(i,j);
		if (dn != 0) {
		    IMAGE(i,j) = (short int)(dn * _mult + _add + 0.5);
		    if (IMAGE(i,j) <= 0)
			IMAGE(i,j) = 1;
		}
	    }
	}
    }
}

////////////////////////////////////////////////////////////////////////
// Write this piece of the model to the label
////////////////////////////////////////////////////////////////////////

int PigBrtCorrLinear::writeToLabel(PigLabelModel *lbl, int index)
{
    return lbl->writeBrtCorrLinearLabel(index, _mult, _add);
}

