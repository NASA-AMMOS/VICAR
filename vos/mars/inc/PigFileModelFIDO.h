////////////////////////////////////////////////////////////////////////
// PigFileModelFIDO
//
// FIDO-specific File model.  This only needs to handle exceptions to the
// multimission label API.
////////////////////////////////////////////////////////////////////////
#ifndef PIGFILEMODELFIDO_H
#define PIGFILEMODELFIDO_H

#include "PigFileModel.h"

#include "fido_lbl_wits_info.h"

class PigFileModelFIDO : public PigFileModel {

  protected:

    FidoLblWitsInfo_typ *_fidoLblWitsInfo;

  public:

    PigFileModelFIDO(const char *filename, int unit, const char *mission);
    virtual ~PigFileModelFIDO();

    virtual const FidoLblWitsInfo_typ *getFidoLblWitsInfo();

    //////////////////////////////////////////////////////////////////////
    // Caller is responsible for memory allocation of uid char array
    // uid array should be big enough to hold at least 33 char
    // For FIDO, for NOW, we'll use productId.  
    // This is not the best choice and might change in the future.
    //////////////////////////////////////////////////////////////////////
    virtual void getUniqueId(char *uid);

    virtual float getInstrumentMastJoint1Angle(float def);
    virtual float getInstrumentMastJoint2Angle(float def);
    virtual float getInstrumentMastJoint3Angle(float def);
    virtual float getInstrumentMastJoint4Angle(float def);

    virtual int checkInstrumentMastJoint1Angle();
    virtual int checkInstrumentMastJoint2Angle();
    virtual int checkInstrumentMastJoint3Angle();
    virtual int checkInstrumentMastJoint4Angle();

    ////////////////////////////////////////////////////////////////////
    // Get the Rover Motion Counter for the file.  The array must be supplied.
    // num_index on input should contain the max size of the supplied array;
    // on output it will contain the number of elements actually found (which
    // could be bigger than the supplied max size, but only that many will
    // actually be returned in the array).  PIG_MAX_SITE_INDEX (from
    // PigRoverStateManager.h) can be useful for array dimensioning.
    ////////////////////////////////////////////////////////////////////

    virtual void getRoverMotionCounter(int indices[], int &num_indices);

    virtual const char *const getModelName() { return "FileModelFIDO"; }

};

#endif

