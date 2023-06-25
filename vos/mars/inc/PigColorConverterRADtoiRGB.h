////////////////////////////////////////////////////////////////////////
// PigColorConverterRADtoiRGB
//
// Sub-class of PigColorConverter which provides the functionality of 
// converting an radiometric corrected image to iRGB using a responsivity
// vector defined in the configuration file.
////////////////////////////////////////////////////////////////////////
#ifndef PIGCOLORCONVERTERRADTOIRGB_H
#define PIGCOLORCONVERTERRADTOIRGB_H

#include "PigColorConverter.h"

class PigColorConverterRADtoiRGB: public PigColorConverter {
    
  protected:
    char *_config_file;  // configuration file
    double _v[3];        // RAD to iRGB vector

    PigColorConverterRADtoiRGB();

  public:
    PigColorConverterRADtoiRGB(char *config_file, char *prefix);

    virtual ~PigColorConverterRADtoiRGB();
    
    // Convert from RAD to iRGB using the vector retrieved with 
    // readRADtoiRGBVector method.
    virtual int convert(SimpleImage<float> *img_in, 
                        SimpleImage<float> *img_out);

    // Read the corresponding RAD to iRGB vector from config file.
    // The vector values are normalized such that green is 1.0.
    virtual void readRADtoiRGBVector(char *prefix);

    // Return the name of the converter.
    virtual const char *const getConverterName()
        { return "PigColorConverterRADtoiRGB"; }
};

#endif
