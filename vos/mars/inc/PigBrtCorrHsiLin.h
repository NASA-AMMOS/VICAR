////////////////////////////////////////////////////////////////////////
// PigBrtCorrHsiLin
//
// Implementation of "HsiLin" brightness correction model.  This is similar
// to Linear in that it applies a constant multiplicative and additive factor
// to the entire image (i.e. i' = i * a + b).  However, this factor is applied
// to the Intensity part of HSI space.  The image is converted to HSI, the
// correction is applied, and then it is converted back to RGB space.
//
////////////////////////////////////////////////////////////////////////
#ifndef PIGBRTCORRHSILIN_H
#define PIGBRTCORRHSILIN_H

#include "PigBrtCorrLinear.h"
#include "RadiometryModel.h"

class PigBrtCorrHsiLin : public PigBrtCorrLinear {

  protected:

    PigFileModel *_file;
    RadiometryModel *_rad;

    PigBrtCorrHsiLin();

    // Note: band is 0-based
    virtual void applyCorrectionInternal(void *image, int max_nl, int max_ns,
					int is_float, int band);
  public:

    // Only the superclass should be calling this.  Don't know why it
    // has to be public but apparently it does...

    PigBrtCorrHsiLin(PigFileModel *file, DOMElement *el, int *status,
			RadiometryModel *rad);

    // Also only for superclass.  This is for a null correction.

    PigBrtCorrHsiLin(PigFileModel *file);

    virtual ~PigBrtCorrHsiLin() { };

    // Print the model, for debugging
    virtual void print();

    // Name for model type in label
    virtual const char *const getModelLabelName() { return "HSI_LIN"; }

    // Write this piece of the model to the label
    virtual int writeToLabel(PigLabelModel *lbl, int index);

    ////////////////////////////////////////////////////////////////////

    virtual const char *const getModelName() { return "PigBrtCorrHsiLin"; }
};

#endif

