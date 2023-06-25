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
#ifndef PIGBRTCORRMODEL_H
#define PIGBRTCORRMODEL_H

#include "PigModelBase.h"
#include "PigXerces.h"

class PigFileModel;
class PigLabelModel;
class RadiometryModel;

class PigBrtCorrModel : public PigModelBase {

  protected:
    char *_mission;		
    char *_instrument;

    int _sl, _ss, _el, _es;	// where the physical image buffer sits
				// relative to the full-frame file coordinates

    // The create() method should normally be used... and the base class
    // should not be instantiated directly.

    PigBrtCorrModel();
    PigBrtCorrModel(const char *mission, const char *instrument,
						int sl, int ss,
						int el, int es);
    PigBrtCorrModel(PigFileModel *file);

    // Note: band is 0-based
    virtual void applyCorrectionInternal(void *image, int max_nl, int max_ns,
					int is_float, int band);
  public:

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

    static void createBrtCorrModels(int nids, PigFileModel *file_models[],
			PigBrtCorrModel *brt_models[], RadiometryModel *rad[]);

    // Create a single BrtCorr model based on the type in the given XML node.
    // Should only be called by createBrtCorrModels() but is public here just
    // in case.

    static PigBrtCorrModel *createOneModel(PigFileModel *, DOMElement *el,
					RadiometryModel *rad);

    virtual ~PigBrtCorrModel();

    // Access functions
    virtual const char *getMissionName() const { return _mission; }
    virtual const char *getInstrument() const { return _instrument; }

    // Apply brightness correction to an image
    // This is the whole purpose of this class.

    // Note: band is 0-based
    virtual void applyCorrection(short int *image,int max_nl,int max_ns,int band)
	{ applyCorrectionInternal((void *)image, max_nl, max_ns, FALSE, band); }
    virtual void applyCorrection(float *image,int max_nl,int max_ns,int band)
	{ applyCorrectionInternal((void *)image, max_nl, max_ns, TRUE, band); }

    ////////////////////////////////////////////////////////////////////

    // Print the model, for debugging
    virtual void print();

    // Name for model type in label
    virtual const char *const getModelLabelName() { return "BASE"; }

    // Write this piece of the model to the label
    virtual int writeToLabel(PigLabelModel *lbl, int index);

    ////////////////////////////////////////////////////////////////////

    virtual const char *const getModelName() { return "PigBrtCorrModel"; }
};

#endif

