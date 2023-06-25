//
// This class manages Rover State (position and orientation) information,
// including info from XML RMC files and labels.  It may be subclassed
// per mission, but each mission has just one of these.
//
// Before use, the RSM object must be set up, where you provide XML files
// and/or labels to ingest.  Generally, all setup should be done before use,
// although this is not a requirement (retrieving information simply reflects
// the state of setup at the time the request is issued).
//
// The RSM object maintains all information in memory; the setup files are
// not used after they are read.
////////////////////////////////////////////////////////////////////////
#ifndef PIGROVERSTATEMANAGER_H
#define PIGROVERSTATEMANAGER_H

#include "PigSolutionManager.h"

#include "PigVector.h"
#include "PigQuaternion.h"
#include "PigXerces.h"

class PigCSDefinition;
class PigMission;

#define PIG_CS_EPSILON 1.0e-3	/* match criteria for comparing values */

class PigRoverStateManager : public PigSolutionManager {

  protected:

    // Static variables to control RMC comparisons.  Must be static because
    // compareIndices is.  These values are obtained from parameters when
    // initialized (see doInit()).

    static int _initialized;
    static int _rmc_max_index;
    static double _epsilon;

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
    //    RMC_EPSILON - Epsilon value for comparison of coordinate sys values.
    //                  Even if RMC's match, a new CS is created if the values
    //                  differ by more than this epsilon.  Setting it high means
    //                  more CS's will be considered equal.
    //                  Default: 1e-3 (PIG_CS_EPSILON macro)

    static void doInit();

    // Print interesting info for a single solution.

    virtual void printSolution(PigSolutionItem *solution);

    // Internal routine to find a solution within one priority.  See the
    // public findSolution().  Returns NULL if no appropriate match is found.

    virtual PigCSDefinition *findSolutionInternal(
			PigCSReference *ident, PigSolutionList *list);

    // Internal routine to find a potential Fixed CS.

    virtual PigCSDefinition *findPotentialFixedCSInternal(
                                        int req_site, PigSolutionList *list);

    // Internal routine to do last-ditch search of References

    virtual PigCSReference *findSolutionRefInternal(
                        PigCSReference *ident, PigSolutionList *list);

    // Convert XML to a PigCSDefinition.

    virtual PigSolutionItem *xmlToItem(DOMElement *solution);

    // Hook for doing special things after a solution is added.  For
    // RSM, this adds extra constant coord system definitions
    // into the database when a related one is added.
    
    virtual void addSpecialSolutions(PigSolutionItem *item);

  public:

    PigRoverStateManager(PigMission *m);
    virtual ~PigRoverStateManager();

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
    // Returns a PigCSDefinition representing this solution, or NULL if
    // not found.

    virtual PigCSDefinition *findSolution(PigCSReference *ident);

    // Fixed frame is defined as a single RMC index with the lowest value.
    // See if there's a better one in the RMC database than what we've already
    // found.
    // If req_site is not -1, it's the requested site # - return that if found

    virtual PigCSDefinition *findPotentialFixedCS(int found_site, int req_site);

    // Hacky routine to do last-ditch search of References for a fixed
    // frame if we can't find anything else

    virtual PigCSReference *findSolutionRef(PigCSReference *ident);

    // Read all coord sys objects in the given file and add them to the
    // in-memory database.

    virtual void addFileCoordSystems(PigFileModel *file);

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

    virtual void addSolution(PigSolutionItem *item);

    // Wrapper around addSolution() that makes more sense from an API
    // standpoint (and for backwards compatibility).
    // ... I have NO idea why the cast is needed ... it shouldn't be!

    virtual void addCoordSys(PigCSDefinition *csdef)
	{ addSolution((PigSolutionItem *)csdef); }

    // Utility routine that parses numbers from a given string.  The string 
    // must contain "(n,n,n)" with anything (except a parenthesis) before or 
    // after, and any number of n's (up to PIG_MAX_CS_INDEX of course).
    // num_index is the size of the array on input, and contains the total 
    // number of indices on output (up to PIG_MAX_CS_INDEX).  Warnings are 
    // printed for parse errors.  As a special case, "()" is allowed, to 
    // indicate no elements.

    static void getIndexFromString(const char *string, int *index,
							int &num_index);

    // Utility routine to return the offset vector from a solution.
    // Quietly returns a 0 vector if there are problems, although some
    // components may be set.

    static PigVector getSolutionVector(DOMElement *solution);

    // Utility routine to return the quaternion from a solution.
    // Quietly returns an identity quaternion if there are any problems.
    // A partial quaternion is never returned.

    static PigQuaternion getSolutionQuaternion(DOMElement *solution);

    // Compare two sets of indices.  Returns -1 if the first is less than
    // the second, 0 of they are equal, and 1 if the first is greater than
    // the second (a la strcmp()).  The shorter set is filled with 0's
    // (e.g. (3,1,5) compared to (3,1), the second is filled to (3,1,0)).
    // Only a maximum of _rmc_max_index indices are compared, allowing 
    // low-order parts of the RMC to be ignored.  Also, any values with -1 
    // are considered wildcards which match anything.  If mask is given,
    // anything marked -1 in there is also wildcarded.

    static int compareIndices(const int *set1, int count1,
			const int *set2, int count2,
			const int *mask, int mask_count);

    // Get the max # of RMC indices to use

    static int getRmcMaxIndex() {
	if (!_initialized) doInit();
	return _rmc_max_index;
    }

};

#endif

