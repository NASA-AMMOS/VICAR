////////////////////////////////////////////////////////////////////////
// PigLabelModelMER
//
// MER-specific class for Label models.  Responsible for managing label
// information for the output file.
//
////////////////////////////////////////////////////////////////////////
#ifndef PIGLABELMODELMER_H
#define PIGLABELMODELMER_H

#include "PigLabelModel.h"

class PigLabelModelMER : public PigLabelModel {

 protected:
    
    ///////////////////////////////////////////////////////////////////////////
    // Returns Rover Motion Counter(RMC) index name.  For MER, we have 
    // 6 indexes,
    // named (SITE, DRIVE, IDD, PMA, HGA, TWEAK).  Other missions might have
    // different number or names of indexes, so this function should be 
    // overloaded.
    //////////////////////////////////////////////////////////////////////////
    virtual char* getCoordSysIndexName(int index);
    
 public:

    // Constructor should only be called via PigMission's create functions!
    PigLabelModelMER(int unit, const char *mission);
    virtual ~PigLabelModelMER();
    
    ////////////////////////////////////////////////////////////////////////
    // Write marsreach's labels to the output file.
    ////////////////////////////////////////////////////////////////////////
    virtual int setReach(PigFileModel *file_models[], int nids);

    ////////////////////////////////////////////////////////////////////////
    // Write marsrough's labels to the output file.  Flag is ignored.
    ////////////////////////////////////////////////////////////////////////
    virtual int setRough(PigFileModel *file_models[], int nids,
                         PigCoordSystem *cs, float invalid_constant, int flag);


    virtual const char *const getModelName() { return "LabelModelMER"; }

};

#endif
