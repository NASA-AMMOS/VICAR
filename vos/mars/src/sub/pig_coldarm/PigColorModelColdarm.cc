////////////////////////////////////////////////////////////////////////
// PigColorModelColdarm
//
// subclass of color model for Coldarm.
////////////////////////////////////////////////////////////////////////

#include "PigColorConverter.h"
#include "PigColorConverterMatrix.h"
#include "PigColorConverterRADtoiRGB.h"
#include "PigColorModelColdarm.h"
#include "PigFileModel.h"
#include "PigMission.h"
#include "PigXerces.h"
#include "PigCameraMapper.h"
#include "PigCameraMapEntry.h"
#include <stdlib.h>

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigColorModelColdarm::PigColorModelColdarm(const char *mission,
                                   const char *instrument, 
                                   const char *host_id,
                                   double dynamic_color_scaling_factor)
    : PigColorModel(mission, instrument)
{
    _host_id = _prefix = NULL;

    if (host_id) _host_id = strdup(host_id);
    _prefix = (char *)malloc(64);
    strcpy(_prefix, "\0");

    if (constructPrefix()) {
        // partial path to the file that defines all NSYT color matrices.
        char *config_file = "param_files/M20_color_cal.parms";

        if (_do_dynamic_color_scaling) {
            _color_dn_scaling_factor = dynamic_color_scaling_factor;
        } else {
            // read SN_XXXX_color_dn_exp_scaling_factor in the config file
            _color_dn_scaling_factor = getStaticColorScalingFactor(config_file,
                                                                   _prefix);
        }

        // init color space converters
        PigColorConverter *pcc_irgb_to_xyz = new PigColorConverterMatrix("iRGB",
            "XYZ", config_file, _prefix, _color_dn_scaling_factor, TRUE);
        PigColorConverter *pcc_irgb_to_wrgb = new PigColorConverterMatrix("iRGB",
            "wRGB", config_file, _prefix, _color_dn_scaling_factor, TRUE);
        PigColorConverter *pcc_srgb_to_prgb = new PigColorConverterMatrix("sRGB",
            "pRGB", config_file, _prefix, _color_dn_scaling_factor, FALSE);
        PigColorConverter *pcc_xyz_to_prgb = new PigColorConverterMatrix("XYZ",
            "pRGB", config_file, _prefix, _color_dn_scaling_factor, FALSE);
        PigColorConverter *pcc_rad_to_irgb = new PigColorConverterRADtoiRGB(
            config_file, _prefix); 

        if (pcc_irgb_to_xyz->isValid())
            _pcc_array[_count++] = pcc_irgb_to_xyz;
        if (pcc_irgb_to_wrgb->isValid())
            _pcc_array[_count++] = pcc_irgb_to_wrgb;
        if (pcc_srgb_to_prgb->isValid())
            _pcc_array[_count++] = pcc_srgb_to_prgb;
        if (pcc_rad_to_irgb->isValid())
            _pcc_array[_count++] = pcc_rad_to_irgb;
    }
}

PigColorModelColdarm::PigColorModelColdarm() : PigColorModel()
{
    _host_id = _prefix = NULL;
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigColorModelColdarm::~PigColorModelColdarm()
{
    if (_host_id) delete _host_id;
    if (_prefix) delete _prefix;
}

////////////////////////////////////////////////////////////////////////
// This factory method creates and returns an instance of this
// subclass for the camera associated with the given file (determined
// by the labels).
////////////////////////////////////////////////////////////////////////

PigColorModelColdarm *PigColorModelColdarm::create(PigFileModel *file)
{
    PigMission *m = PigMission::getMissionObject(file);
    return new PigColorModelColdarm(file->getMissionName(),
                                file->getInstrumentId(),
                                m->getHostID(),
                                getDynamicColorScalingFactor(file));
}

////////////////////////////////////////////////////////////////////////
// Construct the prefix of keys used in config file to find color matrix
////////////////////////////////////////////////////////////////////////

int PigColorModelColdarm::constructPrefix()
{
    // Find out camera's serial number
    PigXerces::initialize();
    PigCameraMapper *map = new PigCameraMapper(NULL, _host_id);
    PigCameraMapEntry *entry = NULL;

    if (_instrument)
        entry = map->findFromID(_instrument);

    if (entry && entry->getSerialNumber()) {
        strcat(_prefix, "SN_");
        strcat(_prefix, entry->getSerialNumber());
        strcat(_prefix, "_");
    } else {
        return FALSE;
    }

    return TRUE;
}

double PigColorModelColdarm::getStaticColorScalingFactor(char *config_file,
                                                     char *prefix)
{
    FILE *inClientFile;
    char pre[256];
    char line[256];
    double static_color_scaling_factor;

    // open the file
    inClientFile = PigModelBase::openConfigFile(config_file, NULL);

    if (inClientFile == NULL) {
        _color_dn_scaling_factor = 1.0;
    } else {
        strcpy(pre, prefix);
        strcat(pre, "color_dn_exp_scaling_factor = %lf");

        while (fgets(line, sizeof(line), inClientFile) != NULL) {
            if (strncasecmp(line, pre, 35) == 0) {
                sscanf(line, pre, &static_color_scaling_factor);
                break;
            }
        }
    }

    return static_color_scaling_factor;
}
