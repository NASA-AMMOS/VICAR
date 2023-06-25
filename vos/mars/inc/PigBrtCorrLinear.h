////////////////////////////////////////////////////////////////////////
// PigBrtCorrLinear
//
// Implementation of "Linear" brightness correction model.  This simply
// applies a constant multiplicative and additive factor to the entire image
// (i.e. i' = i * a + b)
//
////////////////////////////////////////////////////////////////////////
#ifndef PIGBRTCORRLINEAR_H
#define PIGBRTCORRLINEAR_H

#include "PigBrtCorrModel.h"

class PigBrtCorrLinear : public PigBrtCorrModel {

  protected:

    double _add;
    double _mult;

    PigBrtCorrLinear();

    // Note: band is 0-based
    virtual void applyCorrectionInternal(void *image, int max_nl, int max_ns,
					int is_float, int band);
  public:

    // Only the superclass should be calling this.  Don't know why it
    // has to be public but apparently it does...

    PigBrtCorrLinear(PigFileModel *file, DOMElement *el, int *status);

    // Also only for superclass.  This is for a null correction.

    PigBrtCorrLinear(PigFileModel *file);

    virtual ~PigBrtCorrLinear() { };

    ////////////////////////////////////////////////////////////////////
    // Accessor methods.  Should not normally be used; the primary use
    // case for them is marsbrt itself.
    virtual double getAdd() { return _add; }
    virtual double getMult() { return _mult; }

    ////////////////////////////////////////////////////////////////////

    // Print the model, for debugging
    virtual void print();

    // Name for model type in label
    virtual const char *const getModelLabelName() { return "LINEAR"; }

    // Write this piece of the model to the label
    virtual int writeToLabel(PigLabelModel *lbl, int index);

    ////////////////////////////////////////////////////////////////////

    virtual const char *const getModelName() { return "PigBrtCorrLinear"; }
};

#endif

