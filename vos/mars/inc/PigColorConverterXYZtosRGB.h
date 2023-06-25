////////////////////////////////////////////////////////////////////////
// PigColorConverterXYZtoxyY
//
// Sub-class of PigColorConverter which provides the functionality of 
// converting an image from XYZ color space to sRGB color space.
////////////////////////////////////////////////////////////////////////
#ifndef PIGCOLORCONVERTERXYZTOSRGB_H
#define PIGCOLORCONVERTERXYZTOSRGB_H

#include "PigModelBase.h"
#include "PigColorConverter.h"

class PigColorConverterXYZtosRGB: public PigColorConverter {

  protected:
    double _m[3][3]; // D65 XYZ to sRGB matrix 

  public:
    PigColorConverterXYZtosRGB();
 
    virtual ~PigColorConverterXYZtosRGB();

    // convert only from XYZ to D65 linear sRGB (without gamma correction)
    virtual int convert(SimpleImage<float> *img_in,
                        SimpleImage<float> *img_out);
    
    // D65 illuminant
    virtual char *getIlluminant() { return "D65"; }

    virtual const char *const getConverterName() 
        { return "PigColorConverterXYZtosRGB"; }
};

#endif
