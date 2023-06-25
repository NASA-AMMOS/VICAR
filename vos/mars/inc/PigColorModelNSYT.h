////////////////////////////////////////////////////////////////////////
// PigColorModelNSYT
//
// subclass of color model for NSYT cameras.
////////////////////////////////////////////////////////////////////////
#ifndef PIGCOLORMODELNSYT_H
#define PIGCOLORMODELNSYT_H

#include "PigColorModel.h"

class PigColorModelNSYT: public PigColorModel {

  protected:
    char *_host_id;
    char *_prefix; 
    
    PigColorModelNSYT();

  public:

    static PigColorModelNSYT *create(PigFileModel *file);
    
    PigColorModelNSYT(const char *mission, const char *instrument, 
                      const char *host_id, double dynamic_color_scaling_factor);

    virtual const char *const getUnitLabelName() 
        { return "WATT*M**-2*SR**-1*NM**-1"; } 
    
    virtual ~PigColorModelNSYT();

    virtual int constructPrefix();

    virtual double getStaticColorScalingFactor(char *config_file, char *prefix);
};

#endif
