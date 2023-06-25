////////////////////////////////////////////////////////////////////////
// PigColorModelColdarm
//
// subclass of color model for Coldarm.
////////////////////////////////////////////////////////////////////////
#ifndef PIGCOLORMODELCOLDARM_H
#define PIGCOLORMODELCOLDARM_H

#include "PigColorModel.h"

class PigColorModelColdarm: public PigColorModel {

  protected:
    char *_host_id;
    char *_prefix;

    PigColorModelColdarm();

  public:

    static PigColorModelColdarm *create(PigFileModel *file);

    PigColorModelColdarm(const char *mission, const char *instrument, 
                     const char *host_id, double dynamic_color_scaling_factor);

    virtual ~PigColorModelColdarm();

    virtual int constructPrefix();

    virtual double getStaticColorScalingFactor(char *config_file, char *prefix);
};

#endif
