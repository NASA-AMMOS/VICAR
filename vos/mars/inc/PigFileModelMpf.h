////////////////////////////////////////////////////////////////////////
// PigFileModelMpf
//
// MPF-specific File model.  Since MPF does not conform to the multimission
// label API, this class translates the MPF labels into their MM equivalents.
// Although we could create the label structures as needed, it is much
// easier to just do them all at once in the ctor.  And since the Pig
// classes are not the mainline MPF processing routines, this is sufficient.
////////////////////////////////////////////////////////////////////////
#ifndef PIGFILEMODELMPF_H
#define PIGFILEMODELMPF_H

#include "PigFileModel.h"

class PigFileModelMpf : public PigFileModel {

  private:
    int genericLabelsInit();
    int impLabelsInit();
    int roverLabelsInit();

  protected:
    PigQuaternion _quat;	// this value is not in the label API... yet

  public:

    PigFileModelMpf(const char *filename, int unit, const char *mission);

    virtual PigQuaternion getInstLocalLevelQuaternion(PigQuaternion def)
	{ return _quat; }

    virtual const char *const getModelName() { return "FileModelMpf"; }

};

#endif

