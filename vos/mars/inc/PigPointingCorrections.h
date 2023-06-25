////////////////////////////////////////////////////////////////////////
// PigPointingCorrections
//
// This class manages Pointing Correction information from a pointing
// correction XML file.
////////////////////////////////////////////////////////////////////////

#ifndef PIGPOINTINGCORRECTIONS_H
#define PIGPOINTINGCORRECTIONS_H

#include "PigSolutionManager.h"
#include "PigPointingParams.h"
#include "PigPCCameraModel.h"
#include "PigSurfaceModelParams.h"
#include "PigXerces.h"

class PigCameraModel;
class PigFileModel;

#define PIGXML_MAX_INPUTS 2000

class PigPCItem : public PigSolutionItem {
  public:
    PigPointingParams *_pointing;
    PigPCCameraModel *_camera;
    char *_priority;
    virtual const char *getPriority() { return _priority; }
    virtual int getSolutionItemType() { return PIG_SOLUTION_TYPE_POINTCORR; }
};

class PigPointingCorrections :public PigSolutionManager {

  protected:

    // Print interesting info for a single solution.

    virtual void printSolution(PigSolutionItem *solution);
    
    // Internal routine to find a solution within one priority.
    // Returns NULL if no appropriate match is found.
    // For Loose match

    virtual PigPCItem *findSolution(const char *name,
				const double params_org[],
                                const double match_tol,
				const char *model_type,
				PigSolutionList *list);

    // Internal routine to find a solution within one priority.  
    // Returns NULL if no appropriate match is found.
    // For Tight match

    virtual PigPCItem *findSolution(const char *name,
				const char *unique_id,
                                const char *model_type,
				PigSolutionList *list);

    // Convert XML to appropriate PigSolutionItem type.  Must be implmented
    // by subclasses.  Create and return the object (caller must free)

    virtual PigSolutionItem *xmlToItem(DOMElement *solution);

  public:

    PigPointingCorrections(PigMission *m);
    virtual ~PigPointingCorrections();

    // Returns list of Pointing model types for a given solution ID and
    // instrument name.  Input strings might be NULL, does not return
    // duplicates of Pointing Model types
    virtual char **getPointingModelTypes(const char *solution_id,
				const char *instrument_name, int &count);

    // Returns total number of solutions ids (don't care about duplicate)
    virtual int getSolutionCount();

    // returns pointing parameters for given image, solution, and model type
    // for "tight" match
    // Returns pointer to internal structure, do not modify or delete!
    virtual PigPointingParams *getPointing(char *unique_id,
			const char *solution_id, const char *model_type);

    // does "loose" match based on given PointingParams and returns any pointing
    // of the given solution that fits
    // Returns pointer to internal structure, do not modify or delete!
    virtual PigPointingParams *getPointing(double params[], double match_tol,
			const char *solution_id, const char *model_type);

    // returns camera model for given image and solution ("tight" match)
    // Returns pointer to internal structure, do not modify or delete!
    virtual PigPCCameraModel *getCameraModel(char *unique_id,
			const char *solution_id, const char *model_type);

    // does "loose" match and returns a matching camera model
    // Returns pointer to internal structure, do not modify or delete!
    virtual PigPCCameraModel *getCameraModel(double params[], double match_tol,
			const char *solution_id, const char *model_type);

};

#endif
