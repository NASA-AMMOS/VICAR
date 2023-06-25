////////////////////////////////////////////////////////////////////////
// RadiometryMpfImp
//
// subclass of Radiometry model for MPF IMP camera.
//
////////////////////////////////////////////////////////////////////////
#ifndef RADIOMETRYMPFIMP_H
#define RADIOMETRYMPFIMP_H

#include <string.h>
#include "RadiometryModel.h"

#define FLAT_NL_MPFIMP 248
#define FLAT_NS_MPFIMP 256

class RadiometryMpfImp: public RadiometryModel {

  protected:

    int _filter;
    float _temperature;

    // static variables to hold flat field buffer & name
    static float _flat[FLAT_NL_MPFIMP][FLAT_NS_MPFIMP];
    static char _flat_tag[256];
    static int _flat_valid;

    RadiometryMpfImp();

  public:

    static RadiometryMpfImp *create(PigFileModel *file);

    RadiometryMpfImp(const char *mission, const char *instrument, int filter,
	float exptime, float temperature, int sl, int ss, int el, int es);

    virtual ~RadiometryMpfImp();

    // These are overridden from the base class to return MPF IMP specific
    // values

    virtual int loadFlatField(float *&flat, int &width, int &height);

    virtual double getResponsivity(int band);
    virtual double getResponsivityFactor(int band);

    // Print the model, for debugging
    virtual void print();

    virtual const char *const getModelName() { return "RadiometryMpfImp"; }
};

#endif

