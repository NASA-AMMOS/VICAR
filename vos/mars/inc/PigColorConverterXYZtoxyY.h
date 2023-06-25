////////////////////////////////////////////////////////////////////////
// PigColorConverterXYZtoxyY
//
// Sub-class of PigColorConverter which provides the functionality of 
// converting an image from XYZ color space to xyY color space.
////////////////////////////////////////////////////////////////////////
#ifndef PIGCOLORCONVERTERXYZTOXYY_H
#define PIGCOLORCONVERTERXYZTOXYY_H

#include "PigModelBase.h"
#include "PigColorConverter.h" 

class PigColorConverterXYZtoxyY: public PigColorConverter {

  public:
    PigColorConverterXYZtoxyY();
    
    virtual ~PigColorConverterXYZtoxyY();
 
    virtual int convert(SimpleImage<float> *img_in, 
                        SimpleImage<float> *img_out);

    virtual const char *const getConverterName() 
        { return "PigColorConverterXYZtoxyY"; }
};

#endif
