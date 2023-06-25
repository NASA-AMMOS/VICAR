////////////////////////////////////////////////////////////////////////
// PigCSReference
//
// Contains all information necessary to uniquely identify a Coordinate
// System.  Does NOT contain any information regarding the definition of
// a CS... just its identity.  (see PigCSDefinition).
////////////////////////////////////////////////////////////////////////

#include "PigCSReference.h"
#include "PigRoverStateManager.h"
#include "PigXerces.h"
#include "PigMission.h"
#include "PigCoordSystem.h"

#include <stdlib.h>
#include <ctype.h>

////////////////////////////////////////////////////////////////////////
// Do all the work of the constuctor
////////////////////////////////////////////////////////////////////////

void PigCSReference::init(PigMission *m,
			const char *name, const char *solution, 
		   	const int* indices, int num_indices,
			const char *inst_id)

{
    _mission = m;
    int status = m->canonicalizeFrameName(name, inst_id,
		_frame_name, _short_name, _max_indices, _mask_indices);
    // message already printed on bad status

    _is_valid = TRUE;
    if (status == 0) {
	_is_valid = FALSE;
    }

    // Limit the index set to the max.  If we have too many that's okay
    // (e.g. we were given the full RMC) but too few is worth a warning.
    // Unless we explicitly null'd out the input RMC.

    if (num_indices > _max_indices)
        num_indices = _max_indices;
    if (num_indices < _max_indices && indices != NULL) {
	char msg[256];
        sprintf(msg, "Frame %s expected %d indices, got %d", _frame_name,
                                _max_indices, num_indices);
        PigModelBase::printStaticMsg(msg, PigMsgWarning);
    }

    if (solution)
	_solution_id = strdup(solution);
    else
	_solution_id = NULL;  

    _num_indices = num_indices;
    for (int i = 0; i < _num_indices; i++)
        _indices[i] = indices[i];

    _full_name = createCSFullName(_frame_name, _num_indices,
				_indices, _solution_id);

    _inst_id = NULL;
    if (inst_id)
	_inst_id = strdup(inst_id);

}

////////////////////////////////////////////////////////////////////////
// Main constructor
////////////////////////////////////////////////////////////////////////

PigCSReference::PigCSReference(PigMission *m,
		   const char *name, const char *solution, 
		   const int* indices, int num_indices, const char *inst_id)
{
    init(m, name, solution, indices, num_indices, inst_id);
}


////////////////////////////////////////////////////////////////////////
// Construct from XML.  If child_name is given, it looks for a child
// named that in the parent (typically "reference_frame"), otherwise it
// looks directly in the parent.  INSTRUMENT is not allowed as a frame name
////////////////////////////////////////////////////////////////////////

PigCSReference::PigCSReference(PigMission *m,
			DOMElement *parent, char *child_name)
{
    _mission = m;

    DOMElement *frame = parent;
    if (child_name != NULL) {
        frame = PigXerces::getOneElement(parent, child_name);
        if (frame == NULL) {
	    init(m, "UNKNOWN", NULL, NULL, 0, NULL);
	    return;			// WHOOPS!!!!
        }
    }

    const char *name = PigXerces::getAttributeCstr(frame, "name");
    if (name == NULL || strlen(name) == 0)
	name = PigXerces::getAttributeCstr(frame, "frame");

    int num = 0;
    int ind[PIG_MAX_CS_INDEX];

    for (int i=0; i < PIG_MAX_CS_INDEX; i++) {
	char ind_str[10];
	sprintf(ind_str, "index%d", i+1);

	ind[num] = PigXerces::getAttributeInt(frame, ind_str,-99999);
	if (ind[num] == -99999) {
	    // Check for one named by RMC
	    const char *rmcname = _mission->getRmcIndexName(i);
	    char rmcnamelow[100];
	    strcpy(rmcnamelow, rmcname);
	    for (int i=0; i<strlen(rmcnamelow); i++) {
		rmcnamelow[i] = tolower(rmcnamelow[i]);
	    }
	    ind[num] = PigXerces::getAttributeInt(frame, rmcnamelow, -99999);
	    if (ind[num] == -99999) {
	        ind[num] = 0;
	        break;				// don't allow holes
	    }
	}
	num++;
    }

    char *solution_id = PigXerces::getAttribute(frame, "solution_id");
    init(m, name, solution_id, ind, num, NULL);
}

////////////////////////////////////////////////////////////////////////
// Copy constructor
////////////////////////////////////////////////////////////////////////

PigCSReference::PigCSReference(const PigCSReference &from)
{
    init(from._mission, from.getFrameName(), from.getSolutionId(),
		from.getIndices(), from.getNumIndices(), from.getInstId());
}

PigCSReference::~PigCSReference()
{
    if (_full_name)
	delete _full_name;
    if (_inst_id)
	delete _inst_id;
}

///////////////////////////////////////////////////////////////////
// Compare two PigCSReference objects.  Return 1 if objects are equal
// 0 if not.   Note that if one Index set is shorter than the other, it
// is filled with 0's.
// (e.g. (3,1,5) compared to (3,1), the second is filled to (3,1,0))
// Also, a null or empty solutionID for "this" object only is treated 
// as a wildcard, matching anything.  Null or empty solutionID for the
// "other" object is treated as identity.
///////////////////////////////////////////////////////////////////
int PigCSReference::isEqual(PigCSReference *csRef)
{		 
    const char *myname = getFrameName();
    const char *refname = NULL;
    if (csRef != NULL)
        refname = csRef->getFrameName();
    if (myname == NULL || refname == NULL)
	return 0;		// no match if no name

    if (strcasecmp(myname, refname))
	return 0;

    char cs_ref_solution[32];
    char rover_site_solution[32];

    // Deal with the intricacies of solution id.  For this object, null or
    // blank is a wildcard, matching anything in csRef.

    if (getSolutionId() == NULL || strlen(getSolutionId()) == 0)
	strcpy(cs_ref_solution, "");
    else 
	strcpy(cs_ref_solution, getSolutionId());

    // For csRef, null or blank is the same as "telemetry"

    if (csRef->getSolutionId() == NULL || strlen(csRef->getSolutionId()) == 0)
	strcpy(rover_site_solution, "telemetry");
    else
	strcpy(rover_site_solution, csRef->getSolutionId());

    // Now check for equality
    if (strlen(cs_ref_solution) != 0 && strcasecmp(cs_ref_solution,
						   rover_site_solution) != 0)
	return 0;

    if (!PigRoverStateManager::compareIndices(_indices, _num_indices,
				csRef->getIndices(), csRef->getNumIndices(),
				_mask_indices, _num_indices))
	return 1;	 // Two objects are equal  

    return 0;		// not equal
}

////////////////////////////////////////////////////////////////////////
// Create a full CS name.  Returns a dyn-alloc string (caller must free!)
// Format is:  NAME:(i,i,...);sol  where ;sol is omitted if NULL.  Note
// that solution id of "telemetry" is treated as NULL.
////////////////////////////////////////////////////////////////////////

char *PigCSReference::createCSFullName(const char *frame_name,
                        int num_indices, const int *indices,
                        const char *solution_id)
{
    char buf[2048];

    strcpy(buf, frame_name);
    strcat(buf, ":(");
    for (int i=0; i<num_indices; i++) {
        if (i != 0)
            strcat(buf, ",");
        char buf2[100];
        sprintf(buf2, "%d", indices[i]);
        strcat(buf, buf2);
    }
    strcat(buf, ")");

    if (solution_id != NULL && strlen(solution_id) != 0) {
        if (strcasecmp(solution_id, "telemetry") != 0) {
            strcat(buf, ";");
            strcat(buf, solution_id);
        }
    }

    return strdup(buf);
}

