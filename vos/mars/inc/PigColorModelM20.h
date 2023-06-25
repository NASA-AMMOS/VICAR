////////////////////////////////////////////////////////////////////////
// PigColorModelM20
//
// subclass of color model for M2020.
////////////////////////////////////////////////////////////////////////
#ifndef PIGCOLORMODELM20_H
#define PIGCOLORMODELM20_H

#include "PigColorModel.h"

class PigColorModelM20: public PigColorModel {

  protected:
    char *_host_id;
    char *_prefix;

    PigColorModelM20();

  public:

    static PigColorModelM20 *create(PigFileModel *file);

    PigColorModelM20(const char *mission, const char *instrument, 
                     const char *host_id, double dynamic_color_scaling_factor);

    virtual ~PigColorModelM20();

    virtual int constructPrefix();

    virtual double getStaticColorScalingFactor(char *config_file, char *prefix);
};

#endif
