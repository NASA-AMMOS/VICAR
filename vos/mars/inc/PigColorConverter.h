////////////////////////////////////////////////////////////////////////
// PigColorConverter
//
// Base class of color converter, which basically does nothing other 
// than defining functions that need to be overwritten in sub-classes. 
////////////////////////////////////////////////////////////////////////
#ifndef PIGCOLORCONVERTER_H
#define PIGCOLORCONVERTER_H

#include "PigModelBase.h"
#include "SimpleImage.h"

class PigColorConverter {

  protected:
    const char *_src_color_space;
    const char *_des_color_space;
    int _is_valid;

    PigColorConverter();
    PigColorConverter(const char *src_color_space, const char *des_color_space);

  public:
    virtual ~PigColorConverter();

    int isConvertible(const char *src_color_space, const char *des_color_space);
    int isValid() { return _is_valid; }

    // sub-classes need to implement this function. 
    virtual int convert(SimpleImage<float> *img_in, 
                        SimpleImage<float> *img_out)  
        { return FALSE; }

    virtual char *getIlluminant() { return NULL; }

    // Access functions
    virtual const char *getSrcColorSpace() { return _src_color_space; }
    virtual const char *getDesColorSpace() { return _des_color_space; }
};

#endif
