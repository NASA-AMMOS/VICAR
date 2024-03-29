////////////////////////////////////////////////////////////////////////
// PigFileModelGeneric
//
// Generic File model.
////////////////////////////////////////////////////////////////////////

#include "PigFileModelGeneric.h"

#include "return_status.h"

////////////////////////////////////////////////////////////////////////
// These functions return complete Label API property structures.
////////////////////////////////////////////////////////////////////////

// Override of base class to read _PARMS.  See comments in .h file.

PIG_READ_LABEL_STRUCTURE(LblInstrumentState_typ, PigFileModelGeneric,
        InstrumentState, "InstrumentState", InstrumentState,
        "INSTRUMENT_STATE_PARMS")

