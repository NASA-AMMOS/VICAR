////////////////////////////////////////////////////////////////////////
// PigLabelModelPHX
//
// PHX-specific class for Label models.  Responsible for managing label
// information for the output file.
//
////////////////////////////////////////////////////////////////////////
#ifndef PIGLABELMODELPHX_H
#define PIGLABELMODELPHX_H

#include "PigLabelModel.h"

class PigLabelModelPHX : public PigLabelModel {

 protected:
    
    ///////////////////////////////////////////////////////////////////////////
    // Returns Rover Motion Counter(RMC) index name.  For PHX, we have 
    // 2 indexes,
    // named (SITE, DRIVE).  Other missions might have different number or
    // names of indexes, so this function should be overloaded.
    ///////////////////////////////////////////////////////////////////////////
    virtual char* getCoordSysIndexName(int index);
    
 public:

    // Constructor should only be called via PigMission's create functions!
    PigLabelModelPHX(int unit, const char *mission);
    virtual ~PigLabelModelPHX();
    
    ////////////////////////////////////////////////////////////////////////
    // Write marsreach's labels to the output file.
    ////////////////////////////////////////////////////////////////////////
    virtual int setReach(PigFileModel *file_models[], int nids);

    virtual const char *const getModelName() { return "LabelModelPHX"; }

};

#endif
