////////////////////////////////////////////////////////////////////////
// PigColorConverterMatrix
//
// Sub-class of PigColorConverter which provides the functionality of 
// converting an image from one color space to another based on a
// given matrix.
////////////////////////////////////////////////////////////////////////

#include "PigModelBase.h"
#include "PigColorConverterMatrix.h"
#include <stdlib.h>

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigColorConverterMatrix::PigColorConverterMatrix(
                                 const char *src_color_space, 
                                 const char *des_color_space,
                                 char *config_file, char *prefix, 
                                 double color_scaling_factor,
                                 short int doColorScaling) 
    : PigColorConverter(src_color_space, des_color_space)
{
    _config_file = NULL;

    if (config_file) 
        _config_file = strdup(config_file);
    else
        _is_valid = FALSE;

    _m[0][0] = _m[0][1] = _m[0][2] = 
    _m[1][0] = _m[1][1] = _m[1][2] =
    _m[2][0] = _m[2][1] = _m[2][2] = 0.0;
    readColorMatrix(prefix);

    _illuminant = NULL;
    readIlluminant(prefix);

    _color_scaling_factor = color_scaling_factor;
    _doColorScaling = doColorScaling;
}

PigColorConverterMatrix::PigColorConverterMatrix() : PigColorConverter()
{
    _config_file = NULL;
    _is_valid = FALSE;
    _illuminant = NULL;
    _color_scaling_factor = 1.0;
    _doColorScaling = FALSE;
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigColorConverterMatrix::~PigColorConverterMatrix()
{
    if (_config_file)
        delete _config_file;
}

////////////////////////////////////////////////////////////////////////
// Convert an image from one color space to another using the color 
// matrix retrieved with readColorMatrix method. Input and output images
// must be exactly the same dimension, and they must be 3-band.
////////////////////////////////////////////////////////////////////////

int PigColorConverterMatrix::convert(SimpleImage<float> *img_in, 
                                     SimpleImage<float> *img_out)
{
    int nl = img_in->getNL();
    int ns = img_in->getNS();
    int nb = img_in->getNB();
    double p, q, r;
    double v1, v2, v3;

    // Input and output images must be exactly the same dimension, 
    // and they must be 3-band.
    if (img_out->getNL() != nl || img_out->getNS() != ns || 
        img_out->getNB() != nb || nb != 3) {
        return FALSE;
    }

    for (int l = 0; l < nl; l++) {
        for (int s = 0; s < ns; s++) {
            v1 = img_in->get(0,l,s);
            v2 = img_in->get(1,l,s);
            v3 = img_in->get(2,l,s);

            p = _m[0][0] * v1 + _m[0][1] * v2 + _m[0][2] * v3;
            q = _m[1][0] * v1 + _m[1][1] * v2 + _m[1][2] * v3;
            r = _m[2][0] * v1 + _m[2][1] * v2 + _m[2][2] * v3;

            if (_doColorScaling) {
                p = p * _color_scaling_factor;
                q = q * _color_scaling_factor;
                r = r * _color_scaling_factor;
            }

            img_out->set(0, l, s, p);
            img_out->set(1, l, s, q);
            img_out->set(2, l, s, r);
        }
    }
 
    return TRUE;
}

////////////////////////////////////////////////////////////////////////
// Read the corresponding color matrix from config file.
////////////////////////////////////////////////////////////////////////

void PigColorConverterMatrix::readColorMatrix(char *prefix)
{
    FILE *inClientFile;
    char line[256];
    char msg[256];
    int cols = 3;  //number of columns in color matrix.
    char pre[256];

    // open the file
    inClientFile = PigModelBase::openConfigFile(_config_file, NULL);

    if (inClientFile == NULL) {
        sprintf(msg, "Configuration file %s could not be opened, using default "
               "identity matrix for %s to %s converter.", _src_color_space, 
               _des_color_space, _config_file);
        PigModelBase::printUniqueStaticMsg(msg, PigMsgWarning);

        // init identity matrix
        _m[0][0] = _m[1][1] = _m[2][2] = 1.0;
    } else {
        // pre should be in the format SN_xxxx_ where xxxx is the camera's
        // serial number.
        strcpy(pre, prefix); 
        strcat(pre, _src_color_space);
        strcat(pre, "_to_");
        strcat(pre, _des_color_space);
        strcat(pre, "_matrix =\n");

        // < 0 or >= 3 means don't read, otherwise read. 
        int readCounter = -1;

        char *lineFormat = "%lf %lf %lf";

        // Go through the file line by line to find the color matrix.
        // If the key of the color matrix is found, then read the next
        // three consecutive lines.
        while (fgets(line, sizeof(line), inClientFile) != NULL) {
            if (readCounter >= 0 && readCounter < 3) {
                sscanf(line, lineFormat, &_m[readCounter][0], 
                                         &_m[readCounter][1], 
                                         &_m[readCounter][2]);
                readCounter++;
                continue;
            }

            if (readCounter == 3) break;

            if (strcasecmp(line, pre) == 0) {
                readCounter = 0; // set read indicator to 0, so it starts reading.
            }
        }

        if (readCounter == 3) {
            // report the matrix used.
            sprintf(msg, "The color matrix used for %s to %s converter:\n"
                    "%f %f %f\n%f %f %f\n%f %f %f", 
                    _src_color_space, _des_color_space,
                    _m[0][0], _m[0][1], _m[0][2], 
                    _m[1][0], _m[1][1], _m[1][2], 
                    _m[2][0], _m[2][1], _m[2][2]);
            PigModelBase::printUniqueStaticMsg(msg, PigMsgInfo);
        } else {
            // if the key is not found in the config file, then we will use
            // identity maxtrix.
            _m[0][0] = _m[1][1] = _m[2][2] = 1.0;

            sprintf(msg, "The color matrix %s is not found for %s to %s converter"
                   ", so use identity matrix", pre, _src_color_space,
                   _des_color_space);
            PigModelBase::printUniqueStaticMsg(msg, PigMsgWarning);
        }
    }
}

////////////////////////////////////////////////////////////////////////
// Read the corresponding illuminant value from config file.
////////////////////////////////////////////////////////////////////////

void PigColorConverterMatrix::readIlluminant(char *prefix)
{
    FILE *inClientFile;
    char pre[256];
    char line[256];
    char illuminant[256];
    
    // open the file
    inClientFile = PigModelBase::openConfigFile(_config_file, NULL);

    if (inClientFile == NULL) {
        _illuminant = NULL;
    } else {
        // construct the key of the corresponding illminant entry
        strcpy(pre, prefix);
        strcat(pre, _des_color_space);
        strcat(pre, "_illuminant = %s");

        while (fgets(line, sizeof(line), inClientFile) != NULL) {
            if (strncasecmp(line, pre, 23) == 0) {
                sscanf(line, pre, illuminant);
                _illuminant = strdup(illuminant);
                break;
            }
        } 
    }
}
