////////////////////////////////////////////////////////////////////////
// PigRoverStateManager
//
// This class manages Rover State information, including info from XML
// files and labels.
////////////////////////////////////////////////////////////////////////

#include "PigRoverStateManager.h"
#include "PigCSDefinition.h"
#include "PigFileModel.h"
#include "PigXerces.h"
#include "PigMission.h"
#include "PigCoordSystem.h"	// just for PIG_MAX_CS_INDEX

#include <string.h>
#include <stdlib.h>
#include <ctype.h>

// Static variables initialization

int PigRoverStateManager::_initialized = FALSE;
int PigRoverStateManager::_rmc_max_index = PIG_MAX_CS_INDEX;
double PigRoverStateManager::_epsilon = PIG_CS_EPSILON;

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigRoverStateManager::PigRoverStateManager(PigMission *m)
		: PigSolutionManager(m, "RMC_file")
{
}

PigRoverStateManager::~PigRoverStateManager()
{
}

////////////////////////////////////////////////////////////////////////
// Initialize the RMC comparison parameters.  These parameters allow one
// to ignore parts of the RMC when building coordinate systems.  So you can
// for example put one site/drive value in an RSF file and have it apply
// to all images in that site/drive regardless of the low-level RMC values.
// To do so, set RMC_MAX_INDEX to 2 and RMC_EPSILON to something large
// enough to cover all the expected variation (or >= 1.0 to effectively
// disable the epsilon check).
// Parameters are in POINT_METHOD:
//    RMC_MAX_INDEX - specifies the max # of indices to use when comparing
//                    RMC's.  Effectively makes the RMC only this long.
//                    Default: 12 (PIG_MAX_CS_INDEX macro)
//    RMC_EPSILON - Epsilon value for comparison of coordinate system values.
//                  Even if the RMC's match, a new CS is created if the values
//                  differ by more than this epsilon.  Setting it high means
//                  more CS's will be considered equal.
//                  Default: 1e-3 (PIG_CS_EPSILON macro)
////////////////////////////////////////////////////////////////////////

void PigRoverStateManager::doInit()
{
    if (_initialized)
	return;
    char point_method[1024], *value;
    int count;

    _initialized = TRUE;	// Static initializers set defaults

    PigModelBase::getStaticParam("POINT_METHOD", point_method, &count, 1, 0);
    if (count == 0)
	return;

    value = PigModelBase::parseParamString(point_method, "RMC_MAX_INDEX");
    if (value != NULL) {
	int v = atoi(value);
	if (v > 0)
	    _rmc_max_index = v;
    }

    value = PigModelBase::parseParamString(point_method, "RMC_EPSILON");
    if (value != NULL) {
	double v = atof(value);
	if (v > 0.0)
	    _epsilon = v;
    }
}

////////////////////////////////////////////////////////////////////////
// Print solutions for a single priority.
////////////////////////////////////////////////////////////////////////

void PigRoverStateManager::printSolution(PigSolutionItem *solution)
{
    char msg[1024];
    char tmp[20];

    if (solution->getSolutionItemType() != PIG_SOLUTION_TYPE_COORD) {
	sprintf(msg, "Internal error: unexpected item type '%d' in PigRoverStateManager::printSolution", solution->getSolutionItemType());
	printError(msg);
	return;
    }
    PigCSDefinition *csdef = (PigCSDefinition *)solution;
    PigCSReference *ident = csdef->getIdentity();

    sprintf(msg, "  %s, ref=%s", ident->getFullName(),
			csdef->getReference() ?
				csdef->getReference()->getFullName() : "NULL");
    printInfo(msg);

}

////////////////////////////////////////////////////////////////////////
// Find the solution that best matches the given coordinate frame name,
// index value, and solution ID.  This implements the algorithm defined
// in the RMC SIS, and is what users should normally call to get an
// appropriate coord sys definition.
//
// If the solution_id is provided, that specific solution is looked for
// first.  If no solution is found, then a warning is printed and we
// proceed to do the same thing as if solution_id is NULL: search in
// priority order.
//
// If there are more RMC entries than asked for in the target, the highest
// RMC across all priorities that is less than or equal to the target RMC
// is returned.  Of course, for equal RMC's, the highest priority solution
// is used... but it is more important to find the highest possible RMC
// than the highest priority.
//
// SPECIAL CASE of the above: if the asked-for solution has 0 index entries,
// it is interpreted as a request for the fixed frame, so we return the
// lowest numbered index1 (should be SITE).
//
// Returns a PigCSDefinition representing this solution, or NULL if not found.
////////////////////////////////////////////////////////////////////////

PigCSDefinition *PigRoverStateManager::findSolution(PigCSReference *ident)
{
    char msg[256];

    PigCSDefinition *best_solution = NULL;
    int best_index[PIG_MAX_CS_INDEX];
    int best_index_size = 0;

    // If a solution ID is given, look for a match with that one only first.

    if (ident->getSolutionId() != NULL && (strlen(ident->getSolutionId())!=0)) {
	int priority = findPriority(ident->getSolutionId());
	if (priority == -1) {
	    sprintf(msg, "Unable to find ANY entries for solution id '%s'.  Using best available.", ident->getSolutionId());
	    printWarning(msg);

	    // Fall through to standard search
	}
	else {
	    PigCSDefinition *solution;
	    solution = findSolutionInternal(ident,
					    &_prioritySolutionList[priority]);
	    if (solution != NULL) {
		return solution;		// found it!
	    }

	    sprintf(msg,
	       "Unable to find matching entry for solution id '%s', frame '%s'",
				ident->getSolutionId(), ident->getFullName());
	    printWarning(msg);
	    printWarning("Using best available");

	    // Fall through to standard search
	}
    }

    // Start with the highest priority and work backwards.  Find the best
    // solution in each priority, then return the "highest" among all those.

    int size = _priorityList.size();
    for (int i=size-1; i >= -1; i--) {
        PigCSDefinition *solution;
	if (i != -1)				// normal priority
	    solution = findSolutionInternal(ident,
						&_prioritySolutionList[i]);
	else					// last-ditch priority (-1)
	    solution = findSolutionInternal(ident,
						&_leftoverSolutionList);
	if (solution != NULL) {

	    // This is the first, or higher than the previous best

	    const int *tmp_index = solution->getIdentity()->getIndices();
	    int tmp_index_size = solution->getIdentity()->getNumIndices();
	    if ((best_solution == NULL) ||
		(compareIndices(tmp_index, tmp_index_size,
				best_index, best_index_size,
				solution->getIdentity()->getMask(),
				solution->getIdentity()->getMaskSize()) > 0)) {

		memcpy(best_index, tmp_index, tmp_index_size * sizeof(int));
		best_index_size = tmp_index_size;
		best_solution = solution;
	    }
	}
    }
    return best_solution;
}

////////////////////////////////////////////////////////////////////////
// Internal routine to find a solution within one priority.  See the
// public findSolution().  Returns NULL if no appropriate match is found.
////////////////////////////////////////////////////////////////////////

PigCSDefinition *PigRoverStateManager::findSolutionInternal(
			PigCSReference *ident, PigSolutionList *list)
{
    if (list == NULL)
	return NULL;

    int num_slns = list->size();
    if (num_slns == 0)
	return NULL;

    PigCSDefinition *best_solution = NULL;
    int best_index[PIG_MAX_CS_INDEX];
    int best_size = 0;

    // If true, we're looking for the lowest Site instead of highest
    int looking_for_fixed = (ident->getNumIndices() == 0);
    int lowest = 1000000;

    for (int i=0; i < num_slns; i++) {

	PigSolutionItem *item = (*list)[i];
	if (item == NULL)
	    continue;			// whoops
	if (item->getSolutionItemType() != PIG_SOLUTION_TYPE_COORD) {
	    char msg[1024];
	    sprintf(msg, "Internal error: unexpected item type '%d' in PigRoverStateManager::findSolutionInternal", item->getSolutionItemType());
	    printError(msg);
	    continue;
	}
	PigCSDefinition *csdef = (PigCSDefinition *)item;

	if (!looking_for_fixed && strcmp(ident->getFrameName(),
				csdef->getIdentity()->getFrameName()) != 0)
	    continue;			// no match
	if (looking_for_fixed && csdef->getIdentity()->getNumIndices() > 1)
	    continue;		// can't be fixed if more than 1 index

	// Simple search if looking for fixed
	if (looking_for_fixed) {
	    // If we actually find something with 0 indices, it's a match...
	    if (csdef->getIdentity()->getNumIndices() == 0) {
		return csdef;
	    }
	    if (csdef->getIdentity()->getIndex(0) < lowest) {
		lowest = csdef->getIdentity()->getIndex(0);
		best_solution = csdef;
	    }
	}
	else {

	    // Note that we don't just use PigCSReference::isEqual() here
	    // because we care about < > too

	    const int *sln_index = csdef->getIdentity()->getIndices();
	    int sln_size = csdef->getIdentity()->getNumIndices();

	    if (compareIndices(sln_index, sln_size,
			ident->getIndices(), ident->getNumIndices(),
			csdef->getIdentity()->getMask(),
			csdef->getIdentity()->getMaskSize()) > 0)
	        continue;		// solution is past the target

	    if ((best_size > 0) &&
	        (compareIndices(sln_index,sln_size, best_index,best_size,
			csdef->getIdentity()->getMask(),
			csdef->getIdentity()->getMaskSize()) < 0))

	        continue;		// solution not as good as best

	    // (note: for csdef == best above, we take csdef as the latest.
	    // Shouldn't happen, but just in case...)
	
	    // We found one...  save it in "best" and keep looking

	    best_solution = csdef;
	    memcpy(best_index, sln_index, sln_size*sizeof(int));
	    best_size = sln_size;

        }
    }

    return best_solution;

}

////////////////////////////////////////////////////////////////////////
// Fixed frame is defined as a single RMC index with the lowest value.
// See if there's a better one in the RMC database than what we've already
// found.
// If req_site is not -1, it's the requested site # - return that if found
////////////////////////////////////////////////////////////////////////

PigCSReference *PigRoverStateManager::findSolutionRef(PigCSReference *ident)
{
    char msg[256];

    PigCSReference *best = NULL;
    int best_site = -1;

    // Start with the highest priority and work backwards.  Find the best
    // solution in each priority, then return the "highest" among all those.

    int size = _priorityList.size();
    for (int i=size-1; i >= -1; i--) {
        PigCSReference *solution;
	if (i != -1)				// normal priority
	    solution = findSolutionRefInternal(ident,
						&_prioritySolutionList[i]);
	else					// last-ditch priority (-1)
	    solution = findSolutionRefInternal(ident,
						&_leftoverSolutionList);
	if (solution != NULL) {

	    if (solution->getNumIndices() == 1) { // potential...

		int potential = solution->getIndex(0);

		if (best_site == -1) {		// nothing yet, so this is best
		    best = solution;
		    best_site = potential;
		} else {
		    if (potential < best_site) {
			best = solution;
			best_site = potential;
		    }
		}
	    }
	}
    }
    return best;
}

////////////////////////////////////////////////////////////////////////
// Internal routine to find a *reference* within one priority.  Intended
// only for fixed-frame searches.  Returns NULL if no appropriate match
// is found.  This is a last-ditch effort if we can't find a fixed frame
// anywhere else.
////////////////////////////////////////////////////////////////////////

PigCSReference *PigRoverStateManager::findSolutionRefInternal(
			PigCSReference *ident, PigSolutionList *list)
{
    if (list == NULL)
	return NULL;

    int num_slns = list->size();
    if (num_slns == 0)
	return NULL;

    int lowest = 1000000;
    PigCSReference *best = NULL;

    // Look at the references rather than the identities.

    for (int i=0; i < num_slns; i++) {

	PigSolutionItem *item = (*list)[i];
	if (item == NULL)
	    continue;			// whoops
	if (item->getSolutionItemType() != PIG_SOLUTION_TYPE_COORD) {
	    char msg[1024];
	    sprintf(msg, "Internal error: unexpected item type '%d' in PigRoverStateManager::findSolutionRefInternal", item->getSolutionItemType());
	    printError(msg);
	    continue;
	}
	PigCSDefinition *csdef = (PigCSDefinition *)item;

	PigCSReference *r = csdef->getReference();
	if (r == NULL)
	    continue;

	if (r->getNumIndices() > 1)
	    continue;		// can't be fixed if more than 1 index

	// If we actually find something with 0 indices, it's a match...
	if (r->getNumIndices() == 0) {
	    return r;
	}

	if (r->getIndex(0) < lowest) {
	    lowest = r->getIndex(0);
	    best = r;
	}
    }

    return best;
}


////////////////////////////////////////////////////////////////////////
// Fixed frame is defined as a single RMC index with the lowest value.
// But if FIXED_NAME is given, look only for that name explicitly.
// See if there's a better one in the RMC database than what we've already
// found.
// If req_site is not -1, it's the requested site # - return that if found
//
// Looks for the following in the PDF (although will work fine without):
// PARM FIXED_NAME TYPE=STRING COUNT=0:1 DEFAULT=--
////////////////////////////////////////////////////////////////////////

PigCSDefinition *PigRoverStateManager::findPotentialFixedCS(int found_site,
							    int req_site)
{
    char msg[256];

    PigCSDefinition *best_solution = NULL;
    int best_site = found_site;

    // See if FIXED_NAME given

    char fixed_name[256];
    int fixed_name_count = 0;
    getParam("FIXED_NAME", fixed_name, &fixed_name_count, 1, 0);

    // Start with the highest priority and work backwards.  Find the best
    // solution in each priority, then return the "highest" among all those.

    int size = _priorityList.size();
    for (int i=size-1; i >= -1; i--) {
        PigCSDefinition *solution;
	if (i != -1)				// normal priority
	    solution = findPotentialFixedCSInternal(req_site,
						&_prioritySolutionList[i]);
	else					// last-ditch priority (-1)
	    solution = findPotentialFixedCSInternal(req_site,
						&_leftoverSolutionList);
	if (solution != NULL) {

	    if (solution->getIdentity()->getNumIndices() == 1) { // potential...

                // If we asked for a name and this ain't it, continue
                if (fixed_name_count != 0 &&
                        strcasecmp(solution->getIdentity()->getFrameName(),
							fixed_name) != 0) {
                    continue;
                }

		int potential = solution->getIdentity()->getIndex(0);

		if (req_site != -1 && req_site == potential) {
		    return solution;	// found the one we're looking for
		}

		if (best_site == -1) {		// nothing yet, so this is best
		    best_solution = solution;
		    best_site = potential;
		} else {
		    if (potential < best_site) {
			best_solution = solution;
			best_site = potential;
		    }
		}
	    }
	}
    }
    return best_solution;
}

////////////////////////////////////////////////////////////////////////
// Internal routine to find a potential Fixed CS.
////////////////////////////////////////////////////////////////////////

PigCSDefinition *PigRoverStateManager::findPotentialFixedCSInternal(
					int req_site, PigSolutionList *list)
{
    if (list == NULL)
	return NULL;

    int num_slns = list->size();
    if (num_slns == 0)
	return NULL;

    PigCSDefinition *best_solution = NULL;
    int best_site = 1000000;

    for (int i=0; i < num_slns; i++) {

	PigSolutionItem *item = (*list)[i];
	if (item == NULL)
	    continue;			// whoops
	if (item->getSolutionItemType() != PIG_SOLUTION_TYPE_COORD) {
	    char msg[1024];
	    sprintf(msg, "Internal error: unexpected item type '%d' in PigRoverStateManager::findSolutionInternal", item->getSolutionItemType());
	    printError(msg);
	    continue;
	}
	PigCSDefinition *csdef = (PigCSDefinition *)item;
	if (csdef->getIdentity()->getNumIndices() != 1)
	    continue;

	int potential = csdef->getIdentity()->getIndex(0);

	if (req_site != -1 && req_site == potential)
	    return csdef;		// found the one we're looking for

	if (potential < best_site) {
	    best_site = potential;
	    best_solution = csdef;
	}
    }
    return best_solution;

}

////////////////////////////////////////////////////////////////////////
// Read all coord sys objects in the given file and add them to the
// in-memory database.
////////////////////////////////////////////////////////////////////////

void PigRoverStateManager::addFileCoordSystems(PigFileModel *file)
{
    PigCSDefinition *cs_defs[MAX_CS_OBJ];
    int num_cs_defs = MAX_CS_OBJ;

    file->openFile();		// just in case... (usually does nothing)
    int unit = file->getUnit();

    PigFileModel::readAllCoordSystems(_mission, unit, num_cs_defs, cs_defs);

    for (int i=0; i < num_cs_defs; i++) {
        addSolution(cs_defs[i]);
    }

}

////////////////////////////////////////////////////////////////////////
// Add a single coord sys definition to the in-memory database.  This
// follows the algorithm described in the MER RMC/SVF/RVF SIS under
// Ingesting an Image:  Find an exact match of solution ID, and
// the highest RMC <= the argument.  If no match found, add it to
// the database.  If found, the actual definition (offset/quat) is
// compared.  If they match within an epsilon, the new CS is ignored.
// If they don't, it is added.
//
// If the solution ID doesn't yet exist, it is added as the HIGHEST
// priority.  That may not always be appropriate; if not, make sure
// the priority exists before calling this.
////////////////////////////////////////////////////////////////////////

void PigRoverStateManager::addSolution(PigSolutionItem *item)
{
    if (item->getSolutionItemType() != PIG_SOLUTION_TYPE_COORD) {
	char msg[1024];
	sprintf(msg, "Internal error: unexpected item type '%d' in PigRoverStateManager::addSolution", item->getSolutionItemType());
	printError(msg);
	return;
    }
    PigCSDefinition *new_cs = (PigCSDefinition *)item;
    PigCSReference *ident = new_cs->getIdentity();
    PigCSReference *ref = new_cs->getReference();

    if (ident->getFrameName() == NULL)
	return;			// can't add it if no name!

    const char *solution_id = ident->getSolutionId();
    if (solution_id == NULL || strlen(solution_id) == 0)
	solution_id = "telemetry";

    // Look for an exact solution (priority) match...

    int prio = findPriority(solution_id);
    if (prio == -1) {			// no match, so add it...
	addPriority(solution_id);
        prio = findPriority(solution_id);		// find it again
        if (prio == -1) {		// huh??!!
	    printError("Internal error in PigRoverStateManager::addCoordSys().  New CS ignored.");
	    return;
	}
    }

    PigCSDefinition *match = findSolutionInternal(ident,
					&_prioritySolutionList[prio]);

    // findSolutionInternal will find approx matches so check to see if it's
    // an actual match...

    if (match != NULL) {
	if (!ident->isEqual(match->getIdentity()))
	    match = NULL;			// nope
    }

    if (match != NULL) {		// Found a match, compare the values

	// Check that the references match

	PigCSReference *match_ref = match->getReference();

	if (ref->isEqual(match_ref)) {

	    // They do match, now check values

	    PigVector match_off = match->getOffset();
	    PigQuaternion match_quat = match->getQuaternion();

	    PigVector new_off = new_cs->getOffset();
	    PigQuaternion new_quat = new_cs->getQuaternion();

	    if (!_initialized)
	        doInit();		// set up _epsilon if necessary
	    if (new_off.equalEpsilon(match_off, _epsilon) &&
	        new_quat.equalEpsilon(match_quat,  _epsilon)) {

	        return;		// It matches, so we ignore this entry
	    }
	    // We have a matching CS but different definitions: warning!
	    char msg[1024];
	    sprintf(msg,
		"Warning: CS %s definition duplicated with different values",
		ident->getFullName());
	    printError(msg);
	    sprintf(msg, "Offset old: (%f %f %f) new: (%f %f %f), Quat old: (%f %f %f %f) new: (%f %f %f %f)",
		match_off.getX(), match_off.getY(), match_off.getZ(),
		new_off.getX(), new_off.getY(), new_off.getZ(),
		match_quat.getS(), match_quat.getV().getX(),
		match_quat.getV().getY(), match_quat.getV().getZ(),
		new_quat.getS(), new_quat.getV().getX(),
		new_quat.getV().getY(), new_quat.getV().getZ());
	    printError(msg);

	}
    }

    // No match found, so we add it to the database...
    // since addSolution takes over memory management, we create a copy for it

    PigSolutionManager::addSolution(new PigCSDefinition(*new_cs));

    return;
}

////////////////////////////////////////////////////////////////////////
// Convert XML to a PigCSDefinition
////////////////////////////////////////////////////////////////////////

PigSolutionItem *PigRoverStateManager::xmlToItem(DOMElement *solution)
{
    return new PigCSDefinition(_mission, solution);
}

////////////////////////////////////////////////////////////////////////
// Hook for doing special things after a solution is added.  For
// RSM, this adds extra constant coord system definitions
// into the database when a related one is added.
////////////////////////////////////////////////////////////////////////

void PigRoverStateManager::addSpecialSolutions(PigSolutionItem *item)
{
    if (item->getSolutionItemType() != PIG_SOLUTION_TYPE_COORD) {
	char msg[1024];
	sprintf(msg, "Internal error: unexpected item type '%d' in PigRoverStateManager::addSpecialSolutions", item->getSolutionItemType());
	printError(msg);
	return;
    }
    PigCSDefinition *csdef = (PigCSDefinition *)item;

    // Pawn it off on mission so we don't have to subclass

    _mission->addConstantCoordSystems(this, csdef);
}

////////////////////////////////////////////////////////////////////////
// Utility routine that parses numbers from a given string.  The string
// must contain "(n,n,n)" with anything (except a parenthesis) before or
// after, and any number of n's (up to PIG_MAX_CS_INDEX of course).
// num_index is the size of the array on input, and contains the total
// number of indices on output (up to PIG_MAX_CS_INDEX).  Warnings are
// printed for parse errors.  As a special case, "()" is allowed, to
// indicate no elements.
////////////////////////////////////////////////////////////////////////

#define PARSE_ERROR(s) { sprintf(msg, "Parse error in '%s': %s", string, s); \
		printStaticMsg(msg, PigMsgWarning); \
		return; }

void PigRoverStateManager::getIndexFromString(const char *string,
						 int *index, int &num_index)
{
    char msg[256];
    const char *p = string;

    int max_index = num_index;
    if (max_index > PIG_MAX_CS_INDEX)
	max_index = PIG_MAX_CS_INDEX;
    num_index = 0;

    while (*p != '\0' && *p != '(')
	p++;

    if (*p == '\0')
	PARSE_ERROR("no ( found");

    p++;		// skip the (

    if (*p == ')')	// Nothing but (), so accept it with no elements
	return;

    while (num_index < PIG_MAX_CS_INDEX) {
	if (!isdigit(*p))
	    PARSE_ERROR("digit expected");

	if (num_index < max_index) {
	    index[num_index] = atoi(p);
	}
	num_index++;

	while (isdigit(*p))		// skip the number (+/- not allowed)
	    p++;

	if (*p == ')')
	    return;			// all done!
	if (*p != ',')
	    PARSE_ERROR("missing comma");
	p++;				// skip the comma
    }

    PARSE_ERROR("Too many index values");
}

////////////////////////////////////////////////////////////////////////
// Utility routine to return the offset vector from a solution.
// Quietly returns a 0 vector if there are problems, although some
// components may be set.
////////////////////////////////////////////////////////////////////////

PigVector PigRoverStateManager::getSolutionVector(DOMElement *solution)
{
    double *val;
    double x = 0;
    double y = 0;
    double z = 0;

    DOMElement *vector = PigXerces::getOneElement(solution, "offset");
    if (vector == NULL)
	return PigVector();

    x = PigXerces::getAttributeDouble(vector, "x", 0.0);
    y = PigXerces::getAttributeDouble(vector, "y", 0.0);
    z = PigXerces::getAttributeDouble(vector, "z", 0.0);

    return PigVector(x,y,z);
}

////////////////////////////////////////////////////////////////////////
// Utility routine to return the quaternion from a solution.
// Quietly returns an identity quaternion if there are any problems.
// A partial quaternion is never returned.
////////////////////////////////////////////////////////////////////////

PigQuaternion PigRoverStateManager::getSolutionQuaternion(DOMElement *solution)
{
    char *str;
    double s;
    double v[3];
    PigQuaternion identity;

    DOMElement *vector = PigXerces::getOneElement(solution, "orientation");
    if (vector == NULL)
	return identity;

    s = PigXerces::getAttributeDouble(vector, "s", 1.0);
    v[0] = PigXerces::getAttributeDouble(vector, "v1", 0.0);
    v[1] = PigXerces::getAttributeDouble(vector, "v2", 0.0);
    v[2] = PigXerces::getAttributeDouble(vector, "v3", 0.0);

    return PigQuaternion(s, v);
}

////////////////////////////////////////////////////////////////////////
// Compare two sets of indices.  Returns -1 if the first is less than
// the second, 0 of they are equal, and 1 if the first is greater than
// the second (a la strcmp()).  The shorter set is filled with 0's
// (e.g. (3,1,5) compared to (3,1), the second is filled to (3,1,0)).
// Only a maximum of _rmc_max_index indices are compared, allowing
// low-order parts of the RMC to be ignored.  Also, any values with -1
// are considered wildcards which match anything.  If mask is given,
// anything marked -1 in there is also wildcarded.
////////////////////////////////////////////////////////////////////////

int PigRoverStateManager::compareIndices(const int *set1, int count1,
					 const int *set2, int count2,
					 const int *mask, int mask_count)
{
    if (!_initialized)
	doInit();

    // Simply compare in order.  If at any time a value is < or > than the
    // other, the entire set is < or > than the other.

    int max_count = count1;
    if (count2 > count1)
	max_count = count2;

    if (max_count > _rmc_max_index)
	max_count = _rmc_max_index;

    for (int i=0; i < max_count; i++) {
	int value1 = 0;
	if (i < count1)
	    value1 = set1[i];
	int value2 = 0;
	if (i < count2)
	    value2 = set2[i];

	if (mask != NULL && i < mask_count && mask[i] == -1)
	    continue;			// mask==-1 matches anything

	if (value1 == -1 || value2 == -1)	// wildcards match anything
	    continue;

	if (value1 < value2)
	    return -1;

	if (value1 > value2)
	    return 1;
    }
    return 0;			// all are equal
}

