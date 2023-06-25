/* marscolor */

#include "vicmain_c"
#include "mars_support.h"
#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "RadiometryModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigColorModel.h"
#include "SimpleImage.h"
#include <stdio.h>

// buffer sizes in main program.
#define MAX_INPUTS 1  

// Value of 0 indicates there is no upper-bound for input image size.
#define MAX_NL 0
#define MAX_NS 0

void main44()
{
    const int BUF_LEN = 256;
    char msg[BUF_LEN];
    char mission[64], instrument[64];
    int nl, ns;
    int nids;
    int count;
    int success;
    int band = 0;
    int is_float;
    int bits;
    //int doIntToFloat;

    // Inputs
    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    PigCoordSystem *cs;
    PigColorModel *color_models[MAX_INPUTS];
    int homogeneous_inputs = TRUE;

    // Outputs
    int out_unit[3];
    int out_band[3];

    // input and output images
    SimpleImage<float> *img_in[MAX_INPUTS]; 
    memset(img_in, 0, sizeof(img_in));
    
    zvmessage("MARSCOLOR version 2020-05-11", "");

    // Setup up intial pig models.
    mars_setup(nids, file_models, camera_in, pointing_in, NULL, cs, mission, 
               instrument, homogeneous_inputs, MAX_NL, MAX_NS, MAX_INPUTS);

    // Get the mission pointer.
    PigMission *m = PigMission::getMissionObject(mission);

    // Handle band parameter. If band is not specified, then process and return
    // all bands. If band is speified, all the bands will still be processed,
    // but only the specified band will be returned. 
    int band_count = file_models[0]->getNB(); // all inputs are expected have
                                              // the same number of bands.

    if (band_count == 3) {
        zvp("BAND", &band, &count);
        if (count == 0) {
            // band is not specified, process all 3 bands.
            zvmessage("Process all 3 bands, and all 3 bands will be returned", "");
        } else {
            if (band > band_count) {
                band = 1;
                snprintf(msg, BUF_LEN, "Input band (%d) is greater than number "
                         "of bands in input images. Band set to 1.", band);
                zvmessage(msg, "");
            }
            band_count = 1;
        }
    } else {
        snprintf(msg, BUF_LEN, "Inputs must be 3-band images. %d-band images "
                 "are supplied.", band_count);
        zvmessage(msg, "");
        zabend();
    } 

    // Create color model
    color_models[0] = PigColorModel::create(file_models[0]);

    // Get the format of output image. Check DN_SCALE and warn if bad.
    is_float = zvptst("REAL");
    if (is_float) {
        double dn_scale = color_models[0]->getDnScalingFactor();
        if (dn_scale != 1.0) {
            zvmessage("WARNING: DNSCALE should normally be 1.0 when REAL format "
                      "is used.", "");
            snprintf(msg, BUF_LEN, "Verify this is what you want. Specified "
                     "value %f is used.", dn_scale);
            zvmessage(msg, "");
        }
    }

    // Get the number of bits for outputs.
    zvp("BITS", &bits, &count); 

    // figure out the output format
    char *fmt = "HALF";
    if (is_float) {
        fmt = "REAL";
        bits = 0;
    }
    if (zvptst("BYTE")) {
        fmt = "BYTE";
        bits = 8;
    }

    nl = file_models[0]->getNL();
    ns = file_models[0]->getNS(); 

    SimpleImage<float> *img_out[MAX_INPUTS];
    img_out[0] = new SimpleImage<float>(3, nl, ns);

    //mars_read_inputs(0, 0, file_models, img_in, MAX_NL, MAX_NS, 0, doIntToFloat,
    //                 NULL, NULL);
    mars_read_inputs(0, 0, file_models, img_in, MAX_NL, MAX_NS, 0, NULL, NULL);

    // Read source color space from the label COLOR_SPACE.
    const char *src_color_space = file_models[0]->getColorSpace();

    int is_rad = FALSE;

    if (src_color_space == NULL) {
        src_color_space = "iRGB";
        is_rad = file_models[0]->hasRadiometricCorrectionLabel();
    }
    
    // Hardcoded logic. Whenever CIE_XYZ and CIE_xyY are presented in label
    // COLOR_SPACE, these strings should be converted to XYZ and xyY, respectively.
    if (strcasecmp(src_color_space, "CIE_XYZ") == 0) {
        src_color_space = "XYZ";
    } 
    if (strcasecmp(src_color_space, "CIE_xyY") == 0) {
        src_color_space = "xyY";
    }

    // Read destination color space from the parameter DEST_COLOR.
    const char *des_color_space = NULL;
    if (zvptst("XYZ")) {
        des_color_space = "XYZ";
    } else if (zvptst("wRGB")) {
        des_color_space = "wRGB";
    } else if (zvptst("xyY")) {
        des_color_space = "xyY";
    } else if (zvptst("sRGB")) {
        des_color_space = "sRGB";
    } else if (zvptst("pRGB")) {
        des_color_space = "pRGB";
    } else {
        // The code should never be reached because the parameter is required.
        zvmessage("DEST_COLOR is required, but it is not defined.", "");
        zabend();
    }

    if (is_rad) {
        snprintf(msg, BUF_LEN, "Color space conversion: RAD -> iRGB -> %s\n",
                 des_color_space);
        zvmessage(msg, "");
    } else {
        snprintf(msg, BUF_LEN, "Color space conversion:  %s -> %s\n", 
                src_color_space, des_color_space);
        zvmessage(msg, "");
    }

    success = color_models[0]->applyConversion(src_color_space, des_color_space,
                                               img_in, img_out, is_float, bits, 
                                               is_rad);

    if (!success) {
        zvmessage("Invalid color space conversion.", "");
        zabend();
    }

    zvunit(&out_unit[0], "OUT", 1, NULL);
    zvopen(out_unit[0], "op", "write", "u_ns", ns, "u_nl", nl, "u_nb", band_count,
           "open_act", "sa", "u_org", "bsq", "u_format", "real",
           "o_format", fmt, NULL);
    zvplabel(out_unit[0], 0, 1);
    out_unit[2] = out_unit[1] = out_unit[0];
    out_band[0] = 1;
    out_band[1] = 2;
    out_band[2] = 3;

    // write output label
    char *illuminant = color_models[0]->getIlluminant(src_color_space, 
                                                      des_color_space);
    PigLabelModel *labelModel = m->createLabelModel(out_unit[0]);
    labelModel->setColor(des_color_space, illuminant, bits, file_models, 
                         color_models[0], is_float);

    // write output
    if (band_count == 1) {
        for (int line = 0; line < nl; line++) {
            zvwrit(out_unit[0], img_out[0]->linePtr(band - 1, line), "LINE",
                   line + 1, "BAND", 1, "NSAMPS", ns, NULL);
        }
    } else {
        for (int b = 0; b < 3; b++) {
            for (int line = 0; line < nl; line++) {
                zvwrit(out_unit[b], img_out[0]->linePtr(b, line), "LINE",
                       line + 1, "BAND", out_band[b], "NSAMPS", ns, NULL);
            }
        }
    }

    // Save iRGB if OUT_IRGB is specified.  This is a hack because it depends
    // on an undocumented side effect of PigColorModel... namely, that it does
    // the rad->irgb conversion in-place in img_in. HACK!!!!
    int out_irgb_unit[3];
    char filename[256];
    zvp("OUT_IRGB", filename, &count);
    if (count > 0 && is_rad) {
        zvunit(&out_irgb_unit[0], "OUT_IRGB", 1, "U_NAME", filename, NULL);
        zvopen(out_irgb_unit[0], "op", "write", "u_ns", ns, "u_nl", nl, "u_nb",
           band_count, "open_act", "sa", "u_org", "bsq", "u_format", "real",
           "o_format", "real", NULL);
        zvplabel(out_irgb_unit[0], 0, 1);
        out_irgb_unit[2] = out_irgb_unit[1] = out_irgb_unit[0];

        for (int b = 0; b < band_count; b++) {
            for (int line = 0; line < nl; line++) {
                zvwrit(out_irgb_unit[b], img_in[0]->linePtr(b, line), "LINE",
                       line + 1, "BAND", out_band[b], "NSAMPS", ns, NULL);
            }
        }
    }

}
