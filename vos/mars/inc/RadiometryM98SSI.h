////////////////////////////////////////////////////////////////////////
// RadiometryM98SSI
//
// subclass of Radiometry model for M98 SSI camera.
//
////////////////////////////////////////////////////////////////////////
#ifndef RADIOMETRYM98SSI_H
#define RADIOMETRYM98SSI_H

#include <string.h>

#include "RadiometryModel.h"

#define FLAT_NL_M98SSI 248
#define FLAT_NS_M98SSI 256

class RadiometryM98SSI: public RadiometryModel {

  protected:

    int _filter;
    float _temperature;

    // static variables to hold flat field buffer & name
    static float _flat[FLAT_NL_M98SSI][FLAT_NS_M98SSI];
    static char _flat_tag[256];
    static int _flat_valid;

    RadiometryM98SSI();

  public:

    static RadiometryM98SSI *create(PigFileModel *file);

    RadiometryM98SSI(const char *mission, const char *instrument, int filter,
	float exptime, float temperature, int sl, int ss, int el, int es);

    virtual ~RadiometryM98SSI();

    // These are overridden from the base class to return MPF IMP specific
    // values

    virtual int loadFlatField(float *&flat, int &width, int &height);

    virtual double getResponsivity(int band);
    virtual double getResponsivityFactor(int band);

    // Print the model, for debugging
    virtual void print();

    virtual const char *const getModelName() { return "RadiometryM98SSI"; }
};

#endif

