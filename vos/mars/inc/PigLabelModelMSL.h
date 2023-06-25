////////////////////////////////////////////////////////////////////////
// PigLabelModelMSL
//
// MSL-specific class for Label models.  Responsible for managing label
// information for the output file.
//
////////////////////////////////////////////////////////////////////////
#ifndef PIGLABELMODELMSL_H
#define PIGLABELMODELMSL_H

#include "PigLabelModel.h"

class PigLabelModelMSL : public PigLabelModel {

 protected:
    
    ///////////////////////////////////////////////////////////////////////////
    // Returns RMC index name.  For MSL, we have 10 indices,
    // named (SITE, DRIVE, POSE, ARM, CHIMRA, DRILL, RSM, HGA, DRT, IC).
    // Other missions might have different number or
    // names of indexes, so this function should be overloaded.
    ///////////////////////////////////////////////////////////////////////////
    virtual char* getCoordSysIndexName(int index);
    
 public:

    // Constructor should only be called via PigMission's create functions!
    PigLabelModelMSL(int unit, const char *mission);
    virtual ~PigLabelModelMSL();
    
    ////////////////////////////////////////////////////////////////////////
    // Write reachability product labels to the output file.
    ////////////////////////////////////////////////////////////////////////
    virtual int setReach(PigFileModel *file_models[], int nids);

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
    
    virtual const char *const getModelName() { return "LabelModelMSL"; }

};

#endif
