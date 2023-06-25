////////////////////////////////////////////////////////////////////////
// PigColorConverterXYZtoxyY
//
// Sub-class of PigColorConverter which provides the functionality of
// converting an image from XYZ color space to sRGB color space.
////////////////////////////////////////////////////////////////////////

#include "PigColorConverterXYZtosRGB.h"

////////////////////////////////////////////////////////////////////////
// Constructor. Only calls to the base class's constructor to pass in
// variables.
////////////////////////////////////////////////////////////////////////

PigColorConverterXYZtosRGB::PigColorConverterXYZtosRGB()
    : PigColorConverter("XYZ", "sRGB") {

    // CIE XYZ to sRGB D65 linear transformation matrix
    _m[0][0] =  3.2406255; _m[0][1] = -1.537208 ; _m[0][2] = -0.4986286;
    _m[1][0] = -0.9689307; _m[1][1] =  1.8757561; _m[1][2] =  0.0415175;
    _m[2][0] =  0.0557101; _m[2][1] = -0.2040211; _m[2][2] =  1.0569959;
}

////////////////////////////////////////////////////////////////////////
// Destructor. Nothing to clean up.
////////////////////////////////////////////////////////////////////////

PigColorConverterXYZtosRGB::~PigColorConverterXYZtosRGB() {}

////////////////////////////////////////////////////////////////////////
// Convert an image from XYZ to sRGB color space. Input and output images
// must be exactly the same dimension, and they must be 3-band.
////////////////////////////////////////////////////////////////////////

int PigColorConverterXYZtosRGB::convert(SimpleImage<float> *img_in, 
                                        SimpleImage<float> *img_out)
{
    int nl = img_in->getNL();
    int ns = img_in->getNS();
    int nb = img_in->getNB();
    double r, g, b; // R, G, and B values in linear sRGB color space.
    double X, Y, Z; // X, Y, and Z values in XYZ color space.

    // Input and output images must be exactly the same dimension,
    //  and they must be 3-band.
    if (img_out->getNL() != nl || img_out->getNS() != ns ||
        img_out->getNB() != nb || nb != 3) {
        return FALSE;
    }

    for (int l = 0; l < nl; l++) {
        for (int s = 0; s < ns; s++) {
            X = img_in->get(0, l, s);
            Y = img_in->get(1, l, s);
            Z = img_in->get(2, l, s);

            r = _m[0][0] * X + _m[0][1] * Y + _m[0][2] * Z;
            g = _m[1][0] * X + _m[1][1] * Y + _m[1][2] * Z;
            b = _m[2][0] * X + _m[2][1] * Y + _m[2][2] * Z; 

            img_out->set(0, l, s, r);
            img_out->set(1, l, s, g);
            img_out->set(2, l, s, b);
        }
    }
  
    return TRUE;
}
