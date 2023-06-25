////////////////////////////////////////////////////////////////////////
// PigColorModel
//
// Base class for Color Models.  Responsible for applying color correction
// when requested.
///////////////////////////////////////////////////////////////////////

#include "PigColorModel.h"
#include "PigMission.h"
#include "PigFileModel.h"
#include "PigColorConverterXYZtoxyY.h"
#include "PigColorConverterXYZtosRGB.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

int PigColorModel::_count = 0;

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigColorModel::PigColorModel(const char *mission, const char *instrument) 
    : PigModelBase()
{
    _mission = _instrument = NULL;

    if (mission) _mission = strdup(mission);
    if (instrument) _instrument = strdup(instrument);

    _color_dn_scaling_factor = 1.0;

    _do_dynamic_color_scaling = FALSE;
    char color_scaling_method[1024];
    int count;
    getParam("DN_COLOR", color_scaling_method, &count, 1, 0);
    if (strcmp(color_scaling_method, "DN_COLOR") == 0) {
        _do_dynamic_color_scaling = TRUE;
    }

    // init color converter for XYZ to sRGB color space conversion.
    _pcc_array[_count++] = new PigColorConverterXYZtosRGB();
    _pcc_array[_count++] = new PigColorConverterXYZtoxyY();
}

PigColorModel::PigColorModel()
{
    _mission = _instrument = NULL;
    _color_dn_scaling_factor = 1.0;
    _do_dynamic_color_scaling = FALSE;
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigColorModel::~PigColorModel()
{
    if (_mission) delete _mission;
    if (_instrument) delete _instrument;
}

////////////////////////////////////////////////////////////////////////
// This factory method creates and returns an instance of the
// proper subclass for the given camera.  Cameras are specified
// by the file itself (look at the label to figure it out).
////////////////////////////////////////////////////////////////////////

PigColorModel *PigColorModel::create(const char *filename) 
{
    PigFileModel *file = PigFileModel::create(filename);
    if (file == NULL)
        return NULL;
    PigColorModel *color = create(file);
    delete file;
    return color;
}

PigColorModel *PigColorModel::create(PigFileModel *file)
{
    PigMission *m = PigMission::getMissionObject(file);
    return m->createColorModel(file);
}

double PigColorModel::getDnScalingFactor() {
    static int first_time = TRUE;
    static double factor = 0.0;
    char scale_method[1024];
    char msg[256];
    int count; 

    getParam("DNSCALE_OUT", scale_method, &count, 1, 0);

    if (strcmp(scale_method, "DYNAMIC") == 0) {
        // This in general won't be used, set it to 1.0 now.
        factor = 1.0;
    } else if (strcmp(scale_method, "IDENTITY") == 0) {
        // In this case, no scaling factor should be applied.
        factor = 1.0;
    } else {
        getParam("DNSCALE", &factor, &count, 1, 0);
        if (count == 0) {
            factor = 1.0;
        }
        // apply unit scaling factor
        factor = factor * getUnitScalingFactor();

    }

    if (factor == 0.0) {
        return 1.0;
    }

    return 1.0 / factor;
}

int PigColorModel::applyConversion(const char *src_color_space, 
                                   const char *des_color_space,
                                   SimpleImage<float> *img_in[],
                                   SimpleImage<float> *img_out[],
                                   int is_float, int bits, int is_rad) 
{
    int irgb_success = FALSE;
    int success = FALSE;
    double dn_scale = getDnScalingFactor();
    double dn_offset = getDnScalingOffset();
    double dn;

    if (is_rad) {
        // convert from RAD to iRGB
        for (int i = 0; i < _count; i++) {
            if (_pcc_array[i]->isConvertible("RAD", "iRGB")) {
                irgb_success = _pcc_array[i]->convert(img_in[0], img_in[0]);
                break;
            }
        }

        if (!irgb_success) {
            return FALSE;
        }
    }

    // Perform color conversion
    for (int i = 0; i < _count; i++) {
        if (_pcc_array[i]->isConvertible(src_color_space, des_color_space)) {
            success = _pcc_array[i]->convert(img_in[0], img_out[0]);
            break;
        }
    }

    if (!success) {
        return FALSE;
    }

    // Find out the maximum value can be represent by the total bits.
    int maxval = 0;
    if (bits > 0 && bits <= 31) maxval = (1 << bits) - 1;

    for (int b = 0; b < img_out[0]->getNB(); b++) {
        for (int l = 0; l < img_out[0]->getNL(); l++) {
            for (int s = 0; s < img_out[0]->getNS(); s++) {
                dn = img_out[0]->get(b, l, s);
                dn = (dn - dn_offset) / dn_scale; 

                if (is_float) {
                    img_out[0]->set(b, l, s, dn);
                } else {
                    dn = round(dn); 

                    if (bits > 0 && dn > (double)maxval)
                        dn = (double)maxval;
                    if (bits > 0 && dn < 0.0)
                        dn = 0.0;

                    img_out[0]->set(b, l, s, dn);
                }
            }
        }
    }

    return TRUE;
}

char *PigColorModel::getIlluminant(const char *src_color_space, 
                                   const char *des_color_space)
{
    char *illuminant;

    for (int i = 0; i < _count; i++) {
        if (_pcc_array[i]->isConvertible(src_color_space, des_color_space)) {
            illuminant = _pcc_array[i]->getIlluminant();
   
            break;
        }
    }

    return illuminant;
}

double PigColorModel::getDynamicColorScalingFactor(PigFileModel *file)
{
    return file->getExposureDuration(1000.0) / 1000.0;
}

const char *PigColorModel::getColorDnScalingMethod()
{
    if (_do_dynamic_color_scaling) {
        return "DN_COLOR";
    } else {
        return "EXPOSURE_NORMALIZED_COLOR";
    }
}
