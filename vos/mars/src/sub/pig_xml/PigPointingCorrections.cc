///////////////////////////////////////////////////////////////////////
// PigPointingCorrections
//
// This class manages Pointing Correction information from a pointing
// correction XML file.
////////////////////////////////////////////////////////////////////////

#include "PigSurfaceModelParams.h"
#include "PigPCCameraModel.h"
#include "PigPointingCorrections.h"
#include "PigPointingParams.h"
#include "PigXerces.h"
#include "zvproto.h"

#include <string.h>
#include <stdlib.h>
#include <math.h>

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigPointingCorrections::PigPointingCorrections(PigMission *m)
		: PigSolutionManager(m, "pointing_correction")
{
}

PigPointingCorrections::~PigPointingCorrections()
{
}

////////////////////////////////////////////////////////////////////////
// returns list of PointingModel types (don't return duplicate)
////////////////////////////////////////////////////////////////////////

char **PigPointingCorrections::getPointingModelTypes(const char *solution_id,
				const char *instrument_name, int &count)
{
    char **pointing_model_types = new char *[PIGXML_MAX_INPUTS];
    count = 0;

    for (int i=0; i < _prioritySolutionList.size(); i++) {
	PigSolutionList *list = &_prioritySolutionList[i];

        int num_solutions = list->size();
        if (num_solutions == 0)
            printInfo("  None");
        else {
            for (int j=0; j<num_solutions; j++) {
		PigSolutionItem *item = (*list)[j];
		if (item->getSolutionItemType() !=PIG_SOLUTION_TYPE_POINTCORR) {
		    char msg[256];
		    sprintf(msg, "Internal error: unexpected item type '%d' in PigPointingCorrections::getPointingModelTypes", item->getSolutionItemType());
		    printError(msg);
		    continue;
		}
		PigPCItem *pci = (PigPCItem *)item;
		PigPointingParams *pparam = pci->_pointing;
		if (pparam == NULL)
		    continue;

                if (solution_id != NULL) {
		    if (strcasecmp(solution_id, pci->getPriority()) != 0) {
			continue;		// no match
		    }
		}

		// In principle, we should check for instrument_name here.
		// However, this check was commented out in the old version
		// and instrument is not currently in PigPointingParam so I am
		// leaving it out for now....  rgd 2020-05-15

#if 0		//!!!!		finish converting to new xerces if needed
		DOMElement *tmp_img = PigXerces::getOneElement(solution,
								"image");
                if ((instrument_name!=NULL) && 
		    !((tmp_img.getAttribute("instrument")).equals(instrument_name)))
		    continue;                   // no match
#endif		//!!!!

		const char *pointing_model_type = pparam->getType();
                int found = FALSE;
                for (int k=0; k<count; k++) {
                    if (strcasecmp(pointing_model_types[k],
				 pointing_model_type) == 0) { // find duplicate
                        found = TRUE;
                        break;
                    }
                }
                if (!found) {
                    pointing_model_types[count++] = strdup(pointing_model_type);
                }
            }
        }
    }
    
    return pointing_model_types;
}

////////////////////////////////////////////////////////////////////////
// returns solution count (doesn't care about duplicate)
////////////////////////////////////////////////////////////////////////
int PigPointingCorrections::getSolutionCount()
{
    int num_sols = 0;

    for (int i=0; i<_prioritySolutionList.size(); i++) {
	num_sols += _prioritySolutionList[i].size();
    }
    num_sols += _leftoverSolutionList.size();
  
    return num_sols;
}

//!!!!???? ////////////////////////////////////////////////////////////////////////
//!!!!???? // Handle any subclass-specific elements here.  Subclasses should override
//!!!!???? // this to handle anything other than <priority> and <solution> tags that
//!!!!???? // they care about (e.g. <alias> tags in an RMC file).
//!!!!???? ////////////////////////////////////////////////////////////////////////

//!!!!???? void PigPointingCorrections::processExtraElements(DOMDocument *doc)
//!!!!???? {
//!!!!????     DOMElement *root = doc->getDocumentElement();

//!!!!????     _mission = PigXerces::getAttributeCstr(root, "mission");
//!!!!???? }

////////////////////////////////////////////////////////////////////////
// returns PointingParam for given filename, solution, and model type
// for "tight" match
// Returns pointer to internal structure, do not modify or delete!
////////////////////////////////////////////////////////////////////////

PigPointingParams *PigPointingCorrections::getPointing(char *unique_id,
           const char *solution_id, const char *model_type)
{
    PigPCItem *solution;

    int size = _prioritySolutionList.size();
    for (int i=size-1; i >= -1; i--) {
        if (i != -1)                            // normal priority
            solution = findSolution(solution_id, unique_id, model_type,
                                    &_prioritySolutionList[i]);
        else                                    // last-ditch priority (-1)
            solution = findSolution(solution_id, unique_id, model_type,
                                    &_leftoverSolutionList);

        if (solution != NULL) {
	    return solution->_pointing;
        }
    }  
    return NULL;         // not found
}

////////////////////////////////////////////////////////////////////////
// does "loose" match based on given PointingParams and
// returns any pointing of the given solution that fits.
// Returns pointer to internal structure, do not modify or delete!
////////////////////////////////////////////////////////////////////////

PigPointingParams *PigPointingCorrections::getPointing(double pparams_org[],
           double match_tol, const char *solution_id, const char *model_type)
{
    PigPCItem *solution;

    int size = _prioritySolutionList.size();
    for (int i=size-1; i >= -1; i--) {
        if (i != -1)                            // normal priority
            solution = findSolution(solution_id, pparams_org, match_tol,
					model_type, &_prioritySolutionList[i]);
        else                                    // last-ditch priority (-1)
            solution = findSolution(solution_id, pparams_org, match_tol,
					model_type, &_leftoverSolutionList);

        if (solution != NULL) {
	    return solution->_pointing;
        }
    }
    return NULL;         // not found
}

////////////////////////////////////////////////////////////////////////
// Internal routine to find a solution (loose match) within one priority. 
// Returns NULL if no appropriate match is found.
////////////////////////////////////////////////////////////////////////

PigPCItem *PigPointingCorrections::findSolution(const char *name,
                const double params_org[], const double match_tol,
		const char *model_type, PigSolutionList *list)
{
    if (list == NULL)
        return NULL;

    int num_slns = list->size();

    if (num_slns == 0)
        return NULL;

    int found = FALSE;
    PigPCItem *match_sln = NULL;

    for (int i=0; i<num_slns; i++) {
	PigSolutionItem *sln = (*list)[i];
	if (sln->getSolutionItemType() != PIG_SOLUTION_TYPE_POINTCORR) {
	    char msg[256];
	    sprintf(msg, "Internal error: unexpected item type '%d' in PigPointingCorrections::findSolution(loose)", sln->getSolutionItemType());
	    printError(msg);
	    continue;
        }
	PigPointingParams *pparam = ((PigPCItem *)sln)->_pointing;

        // it should find nav file entries when input solution_id 
        // is not given (i.e. name==NULL)
        if (name != NULL) {
	    if (strcasecmp(sln->getPriority(), name) != 0)
		continue;			// no match
	}

        int num_params = pparam->getPointingParamCount();
        int j;
        for (j=0; j<num_params; j++) {
            if (fabs(pparam->getPointingParameter(j) - params_org[j]) >
								match_tol) {
                break;                      // no match
	    }
        }
        if (j == num_params && num_params != 0) {          // Found a match
            found = TRUE;
            match_sln = (PigPCItem *)sln;
            break;
        }
    }
    // found match solution
    if (found) 
        return match_sln;
    else
        return NULL;
}

////////////////////////////////////////////////////////////////////////
// Internal routine to find a solution (tight match) within one priority.
// Returns NULL if no appropriate match is found.
////////////////////////////////////////////////////////////////////////

PigPCItem *PigPointingCorrections::findSolution(const char *name,
                const char *unique_id, const char *model_type,
		PigSolutionList *list)
{
    if (list == NULL)
        return NULL;

    int num_slns = list->size();

    if (num_slns == 0)
        return NULL;

    int found = FALSE;
    PigPCItem *match_sln = NULL;

    for (int i=0; i<num_slns; i++) {
	PigSolutionItem *sln = (*list)[i];
	if (sln->getSolutionItemType() != PIG_SOLUTION_TYPE_POINTCORR) {
	    char msg[256];
	    sprintf(msg, "Internal error: unexpected item type '%d' in PigPointingCorrections::findSolution(tight)", sln->getSolutionItemType());
	    printError(msg);
	    continue;
        }
	PigPointingParams *pparam = ((PigPCItem *)sln)->_pointing;

        // it should find nav file entries when input solution_id 
        // is not given (i.e. name==NULL)
        if (name != NULL) {
	    if (strcasecmp(sln->getPriority(), name) != 0)
		continue;			// no match
	}

	if (strcmp(pparam->getUniqueId(), unique_id) == 0) {
            found = TRUE;
            match_sln = (PigPCItem *)sln;
            break;
        }
    }
    // found match solution
    if (found)
        return match_sln;
    else
        return NULL;
}

////////////////////////////////////////////////////////////////////////
// returns CameraModel for given unique_id and solution.  tight match
///////////////////////////////////////////////////////////////////////

PigPCCameraModel *PigPointingCorrections::getCameraModel(char *unique_id,
		   const char *solution_id, const char *model_type)
{
    PigPCItem *solution;

    int size = _prioritySolutionList.size();
    for (int i=size-1; i >= -1; i--) {
        if (i != -1)                            // normal priority
            solution = findSolution(solution_id, unique_id, model_type,
                                    &_prioritySolutionList[i]);
        else                                    // last-ditch priority (-1)
            solution = findSolution(solution_id, unique_id, model_type,
                                    &_leftoverSolutionList);

        if (solution != NULL) {
	    return solution->_camera;
        }
    }
    return NULL;
}

////////////////////////////////////////////////////////////////////////
// does "loose" match and returns a matching CM.
////////////////////////////////////////////////////////////////////////

PigPCCameraModel *PigPointingCorrections::getCameraModel(double pparams_org[], 
		                                        double match_tol,
           	                                        const char *solution_id,
							const char *model_type)
{
    PigPCItem *solution;

    int size = _prioritySolutionList.size();
    for (int i=size-1; i >= -1; i--) {
        if (i != -1)                            // normal priority
            solution = findSolution(solution_id, pparams_org, match_tol,
				model_type, &_prioritySolutionList[i]);
        else                                    // last-ditch priority (-1)
            solution = findSolution(solution_id, pparams_org, match_tol,
				model_type, &_leftoverSolutionList);

        if (solution != NULL) {
	    return solution->_camera;
        }
    }
    return NULL;
}

////////////////////////////////////////////////////////////////////////
// Print solutions for a single priority.
////////////////////////////////////////////////////////////////////////

void PigPointingCorrections::printSolution(PigSolutionItem *solution)
{
    char msg[1024];
  
    if (solution->getSolutionItemType() != PIG_SOLUTION_TYPE_POINTCORR) {
	sprintf(msg, "Internal error: unexpected item type '%d' in PigPointingCorrections::printSolution", solution->getSolutionItemType());
	printError(msg);
	return;
    }
    PigPointingParams *pparam = ((PigPCItem *)solution)->_pointing;
    
    sprintf(msg, "  type=%s, solution_id=%s, index=(", pparam->getType(),
						solution->getPriority());

    for (int i=0; i < PIG_MAX_CS_INDEX; i++) {
	char ind[20];
	sprintf(ind, "%d", pparam->getPointingParameter(i));
	if (i != 0)
	    strcat(msg, ",");
	strcat(msg, ind);
    }

    strcat(msg, ")");

    printInfo(msg);
}

////////////////////////////////////////////////////////////////////////
// Convert XML to appropriate PigSolutionItem type.  Must be implmented
// by subclasses.  Create and return the object (caller must free)
////////////////////////////////////////////////////////////////////////

PigSolutionItem *PigPointingCorrections::xmlToItem(DOMElement *solution)
{
    PigPCItem *item = new PigPCItem();
    item->_pointing = NULL;
    DOMElement *pp = PigXerces::getOneElement(solution,"pointing_parameters");
    if (pp != NULL)
        item->_pointing = new PigPointingParams(pp, solution);
    item->_camera = new PigPCCameraModel(solution);
    item->_priority = PigXerces::getAttributeCstr(solution, "solution_id");

    return item;
}

