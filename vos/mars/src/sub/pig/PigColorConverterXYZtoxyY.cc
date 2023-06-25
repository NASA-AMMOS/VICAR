////////////////////////////////////////////////////////////////////////
// PigColorConverterXYZtoxyY
//
// Sub-class of PigColorConverter which provides the functionality of
// converting an image from XYZ color space to xyY color space.
////////////////////////////////////////////////////////////////////////

#include "PigColorConverterXYZtoxyY.h"

////////////////////////////////////////////////////////////////////////
// Constructor. Only calls to the base class's constructor to pass in 
// variables.
////////////////////////////////////////////////////////////////////////

PigColorConverterXYZtoxyY::PigColorConverterXYZtoxyY() 
    : PigColorConverter("XYZ", "xyY") {}

////////////////////////////////////////////////////////////////////////
// Destructor. Nothing to clean up.
////////////////////////////////////////////////////////////////////////
PigColorConverterXYZtoxyY::~PigColorConverterXYZtoxyY() {}

////////////////////////////////////////////////////////////////////////
// Convert an image from XYZ to xyY color space. Input and output images
// must be exactly the same dimension, and they must be 3-band.
////////////////////////////////////////////////////////////////////////

int PigColorConverterXYZtoxyY::convert(SimpleImage<float> *img_in,
                                       SimpleImage<float> *img_out)
{
    int nl = img_in->getNL();
    int ns = img_in->getNS();
    int nb = img_in->getNB();
    double X, Y, Z; //X, Y, and Z values of XYZ color space
    double x, y; //x and y values of xyY color space

    // Input and output images must be exactly the same dimension,
    // and they must be 3-band.
    if (img_out->getNL() != nl || img_out->getNS() != ns || 
        img_out->getNB() != nb || nb != 3) {
        return FALSE;
    }

    for (int l = 0; l < nl; l++) {
        for (int s = 0; s < ns; s++) {
            X = img_in->get(0, l, s);
            Y = img_in->get(1, l, s);
            Z = img_in->get(2, l, s);

            if (X + Y + Z == 0) {
                x = 0; 
                y = 0; 
            } else {
                x = X / (X + Y + Z);
                y = Y / (X + Y + Z);
            }

            img_out->set(0, l, s, x);
            img_out->set(1, l, s, y);
            img_out->set(2, l, s, Y);
        }
    }

    return TRUE;
}
