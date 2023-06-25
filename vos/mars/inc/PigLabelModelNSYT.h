////////////////////////////////////////////////////////////////////////
// PigLabelModelNSYT
//
// NSYT-specific class for Label models.  Responsible for managing label
// information for the output file.
//
////////////////////////////////////////////////////////////////////////
#ifndef PIGLABELMODELNSYT_H
#define PIGLABELMODELNSYT_H

#include "PigLabelModel.h"

class PigLabelModelNSYT : public PigLabelModel {

 protected:
    
    ///////////////////////////////////////////////////////////////////////////
    // Returns RMC index name.  For NSYT, we have 2 indices,
    // named (SITE, DRIVE).
    // Other missions might have different number or
    // names of indexes, so this function should be overloaded.
    ///////////////////////////////////////////////////////////////////////////
    virtual char* getCoordSysIndexName(int index);
    
 public:

    // Constructor should only be called via PigMission's create functions!
    PigLabelModelNSYT(int unit, const char *mission);
    virtual ~PigLabelModelNSYT();
    
#if 0
//!!!! NEEDS TO BE REPLACED FOR INSIGHT
    ////////////////////////////////////////////////////////////////////////
    // Write reachability product labels to the output file.
    ////////////////////////////////////////////////////////////////////////
    virtual int setReach(PigFileModel *file_models[], int nids);
//!!!! NEEDS TO BE REPLACED FOR INSIGHT
#endif

#if 0
//!!!! NEEDS TO BE REPLACED FOR INSIGHT
    ////////////////////////////////////////////////////////////////////////
    // Write marsrough's labels to the output file. Flag indicates Drill or DRT
    // (0 = DRT, 1 = Drill)
    ////////////////////////////////////////////////////////////////////////
    virtual int setRough(PigFileModel *file_models[], int nids,
                         PigCoordSystem *cs, float invalid_constant, int flag);
//!!!! NEEDS TO BE REPLACED FOR INSIGHT
#endif
    
    virtual const char *const getModelName() { return "LabelModelNSYT"; }

};

#endif
