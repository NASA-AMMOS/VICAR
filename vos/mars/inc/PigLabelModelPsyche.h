////////////////////////////////////////////////////////////////////////
// PigLabelModelPsyche
//
// Psyche-specific class for Label models.  Responsible for managing label
// information for the output file.
//
////////////////////////////////////////////////////////////////////////
#ifndef PIGLABELMODELPSYCHE_H
#define PIGLABELMODELPSYCHE_H

#include "PigLabelModel.h"

class PigLabelModelPsyche : public PigLabelModel {

 protected:
    
    ///////////////////////////////////////////////////////////////////////////
    // Returns RMC index name.  For Psyche, we have 2 indices,
    // named (SITE, DRIVE).
    // Other missions might have different number or
    // names of indexes, so this function should be overloaded.
    ///////////////////////////////////////////////////////////////////////////
    virtual char* getCoordSysIndexName(int index);
    
 public:

    // Constructor should only be called via PigMission's create functions!
    PigLabelModelPsyche(int unit, const char *mission);
    virtual ~PigLabelModelPsyche();
    
    ////////////////////////////////////////////////////////////////////////
    // Write reachability product labels to the output file.
    ////////////////////////////////////////////////////////////////////////
    virtual int setReach(PigFileModel *file_models[], int nids);

    ////////////////////////////////////////////////////////////////////////
    // Write goodness product labels to the output file.
    ////////////////////////////////////////////////////////////////////////
    virtual int setGReach(PigFileModel *file_models[], int nids,
                          int *bands, int nbands, bool include_configs);

    ////////////////////////////////////////////////////////////////////////
    // Write preload product labels to the output file.  The third argument
    // indicates the preload type: 2==2-band preload (min,max); 8==8-band
    // preload (per config).  Other values are invalid.
    ////////////////////////////////////////////////////////////////////////
    virtual int setPreload(PigFileModel *file_models[], int nids,
			   int preload_type);

    ////////////////////////////////////////////////////////////////////////
    // Write marsrough's labels to the output file. Flag indicates Drill or DRT
    // (0 = DRT, 1 = Drill)
    ////////////////////////////////////////////////////////////////////////
    virtual int setRough(PigFileModel *file_models[], int nids,
                         PigCoordSystem *cs, float invalid_constant, int flag);
    
    virtual const char *const getModelName() { return "LabelModelPsyche"; }

    const static char* inst_values[];

    const static char* config_values[];

};

#endif
