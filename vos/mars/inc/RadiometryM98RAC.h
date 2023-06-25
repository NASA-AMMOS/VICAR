////////////////////////////////////////////////////////////////////////
// RadiometryM98RAC
//
// subclass of Radiometry model for M98 RAC camera.
//
////////////////////////////////////////////////////////////////////////
#ifndef RADIOMETRYM98RAC_H
#define RADIOMETRYM98RAC_H

#include <string.h>

#include "RadiometryModel.h"

#define FLAT_NL_M98RAC 256
#define FLAT_NS_M98RAC 512

class RadiometryM98RAC: public RadiometryModel {

  protected:

    int _focus_setting;
    float _temperature;

    // static variables to hold flat field buffer & name
    static float _flat[FLAT_NL_M98RAC][FLAT_NS_M98RAC];
    static char _flat_tag[256];
    static int _flat_valid;

    RadiometryM98RAC();

  public:

    static RadiometryM98RAC *create(PigFileModel *file);

    RadiometryM98RAC(const char *mission, const char *instrument, int focus,
	float exptime, float temperature, int sl, int ss, int el, int es);

    virtual ~RadiometryM98RAC();

    // These are overridden from the base class to return MPF IMP specific
    // values

    virtual int loadFlatField(float *&flat, int &width, int &height);

    virtual double getResponsivity(int band);
    virtual double getResponsivityFactor(int band);

    // Print the model, for debugging
    virtual void print();

    virtual const char *const getModelName() { return "RadiometryM98RAC"; }
};

#endif

