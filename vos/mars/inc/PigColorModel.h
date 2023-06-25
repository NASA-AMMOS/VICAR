////////////////////////////////////////////////////////////////////////
// PigColorModel
//
// Base class for Color Models.  Responsible for applying color correction
// when requested.
///////////////////////////////////////////////////////////////////////
#ifndef PIGCOLORMODEL_H
#define PIGCOLORMODEL_H

#include "PigModelBase.h"
#include "PigColorConverter.h"

#define MAX_CONVERTER_SIZE 100

class PigFileModel;

class PigColorModel : public PigModelBase {
    
  protected:
    char *_mission;
    char *_instrument; 
    PigColorConverter *_pcc_array[MAX_CONVERTER_SIZE]; 

    // static variable to keep track of color space converters in this
    // base class and its sub-classes
    static int _count;

    double _color_dn_scaling_factor;
    short int _do_dynamic_color_scaling;    

    // The create() method should normally be used to create 
    // instance of the class.
    PigColorModel();
    PigColorModel(const char *mission, const char *instrument);
    
  public:

    virtual ~PigColorModel();

    static PigColorModel *create(const char *filename);
    static PigColorModel *create(PigFileModel *file);

    static double getDynamicColorScalingFactor(PigFileModel *file);

    // This function should not be normally changed.
    virtual int applyConversion(const char *src_color_space, 
                                const char *des_color_space,
                                SimpleImage<float> *img_in[], 
                                SimpleImage<float> *img_out[],
                                int is_float, int bits, int is_rad);
    virtual char *getIlluminant(const char *src_color_space, 
                                const char *des_color_space);
    virtual double getDnScalingFactor();
    virtual double getUnitScalingFactor() { return 1.0; }
    virtual double getDnScalingOffset() { return 0.0; }

    virtual double getColorDnScalingFactor() 
        { return _color_dn_scaling_factor; }
    virtual const char *getColorDnScalingMethod();

    virtual const char *const getUnitLabelName() 
        { return "WATT*M**-2*SR**-1*uM**-1"; } 

    // Access functions
    virtual const char *getMissionName() const { return _mission; }
    virtual const char *getInstrument() const { return _instrument; }
};

#endif
