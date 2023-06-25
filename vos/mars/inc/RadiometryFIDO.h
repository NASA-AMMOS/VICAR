////////////////////////////////////////////////////////////////////////
// RadiometryFIDO
//
// subclass of Radiometry model for FIDO camera.
//
////////////////////////////////////////////////////////////////////////
#ifndef RADIOMETRYFIDO_H
#define RADIOMETRYFIDO_H

#include <string.h>

#include "RadiometryModel.h"

#define FLAT_NL_FIDO 480
#define FLAT_NS_FIDO 640

class RadiometryFIDO: public RadiometryModel {

  protected:

    int _filter;
    float _temperature;

    // static variables to hold flat field buffer & name
    static float _flat[FLAT_NL_FIDO][FLAT_NS_FIDO];
    static char _flat_tag[256];
    static int _flat_valid;

    RadiometryFIDO();

  public:

    static RadiometryFIDO *create(PigFileModel *file);

    RadiometryFIDO(const char *mission, const char *instrument,int filter,
	float exptime, float temperature, int sl, int ss, int el, int es);

    virtual ~RadiometryFIDO();

    // These are overridden from the base class to return FIDO specific
    // values

    virtual int loadFlatField(float *&flat, int &width, int &height);

    virtual double getResponsivity(int band);

    // Print the model, for debugging
    virtual void print();

    virtual const char *const getModelName() { return "RadiometryFIDO"; }
};

#endif

