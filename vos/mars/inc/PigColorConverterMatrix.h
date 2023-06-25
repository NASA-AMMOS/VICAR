////////////////////////////////////////////////////////////////////////
// PigColorConverterMatrix
//
// Sub-class of PigColorConverter which provides the functionality of 
// converting an image from one color space to another based on a
// given matrix.
////////////////////////////////////////////////////////////////////////
#ifndef PIGCOLORCONVERTERMATRIX_H
#define PIGCOLORCONVERTERMATRIX_H

#include "PigColorConverter.h"

class PigColorConverterMatrix: public PigColorConverter {

  protected:
    char *_config_file; 
    char *_illuminant;
    double _m[3][3]; // 3 x 3 color matrix.
    double _color_scaling_factor;
    short int _doColorScaling; 
                                                 
    PigColorConverterMatrix();

  public:
    PigColorConverterMatrix(const char *src_color_space, 
                            const char *des_color_space,
                            char *config_file, char *prefix, 
                            double color_scaling_factor, 
                            short int doColorScaling);

    virtual ~PigColorConverterMatrix();
    
    // Convert an image from one color space to another using the 
    // color matrix retrieved with readColorMatrix method.
    virtual int convert(SimpleImage<float> *img_in, 
                        SimpleImage<float> *img_out);

    // Read the corresponding color matrix from config file.
    virtual void readColorMatrix(char *prefix);

    // Read the corresponding illuminant value from config file.
    virtual void readIlluminant(char *prefix);

    // Access function
    virtual char *getIlluminant() { return _illuminant; }

    // Return the name of the converter
    virtual const char *const getConverterName() 
        { return "PigColorConverterMatrix"; }
};

#endif
