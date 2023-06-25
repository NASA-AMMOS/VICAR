////////////////////////////////////////////////////////////////////////
// PigColorConverterRADtoiRGB
//
// Sub-class of PigColorConverter which provides the functionality of
// converting an radiometric corrected image to iRGB using a responsivity
// vector defined in the configuration file.
////////////////////////////////////////////////////////////////////////

#include "PigModelBase.h"
#include "PigColorConverterRADtoiRGB.h"
#include <stdlib.h>

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigColorConverterRADtoiRGB::PigColorConverterRADtoiRGB(char *config_file,
                                                       char *prefix)
    : PigColorConverter("RAD", "iRGB")
{
    _config_file = NULL;

    if (config_file)
        _config_file = strdup(config_file);
    else 
        _is_valid = FALSE;

    _v[0] = _v[1] = _v[2] = 0.0;
    readRADtoiRGBVector(prefix);
}

PigColorConverterRADtoiRGB::PigColorConverterRADtoiRGB() : PigColorConverter()
{
    _config_file = NULL;
    _is_valid = FALSE;
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigColorConverterRADtoiRGB::~PigColorConverterRADtoiRGB()
{
    if (_config_file)
        delete _config_file;
}

////////////////////////////////////////////////////////////////////////
// Convert from RAD to iRGB using the vector retrieved with 
// readRADtoiRGBVector method. Input and output images must be exactly 
// the same dimension, and they must be 3-band.
////////////////////////////////////////////////////////////////////////

int PigColorConverterRADtoiRGB::convert(SimpleImage<float> *img_in,
                                        SimpleImage<float> *img_out)
{
    int nl = img_in->getNL();
    int ns = img_in->getNS();
    int nb = img_in->getNB();

    // Input and output images must be exactly the same dimension,
    // and they must be 3-band.
    if (img_out->getNL() != nl || img_out->getNS() != ns ||
        img_out->getNB() != nb || nb != 3) {
        return FALSE;
    }

    for (int l = 0; l < nl; l++) {
        for (int s = 0; s < ns; s++) {
            img_out->set(0, l, s, img_in->get(0, l, s) / _v[0]);
            img_out->set(1, l, s, img_in->get(1, l, s) / _v[1]);
            img_out->set(2, l, s, img_in->get(2, l, s) / _v[2]);
        }
    }

    return TRUE;
}

void PigColorConverterRADtoiRGB::readRADtoiRGBVector(char *prefix)
{
    FILE *inClientFile;
    char line[256];
    char msg[256];
    char pre[256];
    short int found = FALSE;

    // open the config file
    inClientFile = PigModelBase::openConfigFile(_config_file, NULL);

    if (inClientFile == NULL) {
        // default values for the vector
        _v[0] = _v[1] = _v[2] = 1.0;

        sprintf(msg, "Configuration file %s could not be opened, using default "
                "values %f %f %f for RAD to iRGB conversion.", _config_file, 
                _v[0], _v[1], _v[2]);
        PigModelBase::printUniqueStaticMsg(msg, PigMsgWarning);
    } else {
        // pre should be in the format SN_xxxx_ where xxxx is the camera's
        // serial number.
        strcpy(pre, prefix);
        strcat(pre, "RAD_to_iRGB_vector = %lf %lf %lf");

        while (fgets(line, sizeof(line), inClientFile) != NULL) {
            if (strncasecmp(line, pre, 26) == 0) {
                found = TRUE;
                sscanf(line, pre, &_v[0], &_v[1], &_v[2]);
                break;
            }
        }
    }

    // close file
    fclose(inClientFile);

    if (found) {
        sprintf(msg, "RAD to iRGB conversion values: %e %e %e", 
                _v[0], _v[1], _v[2]);
        PigModelBase::printUniqueStaticMsg(msg, PigMsgInfo);
    } else {
        _v[0] = _v[1] = _v[2] = 1.0;
        sprintf(msg, "RAD to iRGB parameters were not found in %s, using "
                "default values %f %f %f.", _config_file, _v[0], _v[1], _v[2]);
        PigModelBase::printUniqueStaticMsg(msg, PigMsgWarning);
    }
}
