////////////////////////////////////////////////////////////////////////
// PigColorModelPsyche
//
// subclass of color model for Psyche.
////////////////////////////////////////////////////////////////////////
#ifndef PIGCOLORMODELPSYCHE_H
#define PIGCOLORMODELPSYCHE_H

#include "PigColorModel.h"

class PigColorModelPsyche: public PigColorModel {

  protected:
    char *_host_id;
    char *_prefix;

    PigColorModelPsyche();

  public:

    static PigColorModelPsyche *create(PigFileModel *file);

    PigColorModelPsyche(const char *mission, const char *instrument, 
                     const char *host_id, double dynamic_color_scaling_factor);

    virtual ~PigColorModelPsyche();

    virtual int constructPrefix();

    virtual double getStaticColorScalingFactor(char *config_file, char *prefix);
};

#endif
