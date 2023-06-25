////////////////////////////////////////////////////////////////////////
// PigFileModelGeneric
//
// Generic-mission File model.
////////////////////////////////////////////////////////////////////////
#ifndef PIGFILEMODELGENERIC_H
#define PIGFILEMODELGENERIC_H

#include "PigFileModel.h"

class PigFileModelGeneric : public PigFileModel {

  protected:

  public:

    PigFileModelGeneric(const char *filename, int unit, const char *mission)
	: PigFileModel(filename, unit, mission)  {};

    virtual ~PigFileModelGeneric() {};

    // This function is the only reason we're here.  Old missions (pre-MER)
    // use INSTRUMENT_STATE; MER and later use INSTRUMENT_STATE_PARMS.
    // It is unfortunately not feasible in the label API to look for
    // non-PARMS first and then _PARMS if not found; the label API doesn't
    // provide proper return status info for that.  (if it returned a count of
    // found items, that would work, simply check for 0).
    // As a workaround, we look for the _PARMS version here.  This enables
    // use of the generic mission with modern data.

    virtual const LblInstrumentState_typ *getLblInstrumentState();

    virtual const char *const getModelName() { return "FileModelGeneric"; }

};

#endif

