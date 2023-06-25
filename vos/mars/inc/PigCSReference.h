////////////////////////////////////////////////////////////////////////
// PigCSReference
//
// Contains all information necessary to uniquely identify a Coordinate 
// System.  Does NOT contain any information regarding the definition of
// a CS... just its identity.  (see PigCSDefinition).
////////////////////////////////////////////////////////////////////////

#ifndef PIGCSREFERENCE_H
#define PIGCSREFERENCE_H

#include <string.h>
#include "PigXerces.h"
#include "PigRoverStateManager.h"

class PigCSReference {

  protected:
    PigMission *_mission;
    char *_frame_name;			// static alloc
    char *_short_name;			// static alloc
    char *_solution_id;
    char *_full_name;			// dynamic alloc
    int _indices[PIG_MAX_CS_INDEX];
    int _num_indices;
    int _max_indices;
    int _mask_indices[PIG_MAX_CS_INDEX];
    char *_inst_id;			// dynamic alloc
    int _is_valid;

    void init(PigMission *m, const char *name, const char *solution,
		const int* indices, int num_indices, const char *inst_id);

    // Create a full CS name.  Returns a dyn-alloc string (caller must free!)
    // Format is:  NAME:(i,i,...);sol  where ;sol is omitted if NULL.  Note
    // that solution id of "telemetry" is treated as NULL.

    static char *createCSFullName(const char *frame_name, int num_indices,
                        const int *indices, const char *solution_id);

  public:

    // Main constructor

    PigCSReference(PigMission *m, const char *name, const char *solution,
		const int* indices, int num_indices, const char *inst_id);

    // Construct from XML.  If child_name is given, it looks for a child
    // named that in the parent (typically "reference_frame"), otherwise it
    // looks directly in the parent.  INSTRUMENT is not allowed a a frame name

    PigCSReference(PigMission *m, DOMElement *parent, char *child_name);

    // Empty constrctor. Fields must be filled in before use

    PigCSReference(PigMission *m) { init(m, "UNKNOWN", NULL, NULL, 0, NULL); }

    // Really empty constructor
    // We really shouldn't need this... but the existing API uses it
    // in some places.  Only to be copied into with a copy ctor later.
    // So really... don't use this unless you have to.

    PigCSReference() 
	{ _mission = NULL;
	  _frame_name = NULL;
	  _short_name = NULL;
	  _solution_id = NULL;
	  _full_name = NULL;
	  _num_indices = 0;
	  _max_indices = 0;
	  _inst_id = NULL;
	}

    // We don't have construct from label because ident and ref have very
    // different labels...

    // Copy constructor

    PigCSReference(const PigCSReference &from);

    ~PigCSReference();

    ////////////////////////////////////////////////////////////////////
    // Accessor functions.  They all return pointers to internal strings
    // so use immediately or copy
    ////////////////////////////////////////////////////////////////////

    const char *getFrameName() const { return _frame_name; }
    const char *getFullName() const { return _full_name; }
    const char *getShortName() const { return _short_name; }
    const char *getSolutionId() const { return _solution_id; }
    int getNumIndices() const { return _num_indices; }
    int getMaxIndices() const { return _max_indices; }
    const int *getIndices() const { return _indices; }
    int getIndex(int i) const { return _indices[i]; }
    const int *getMask() const { return _mask_indices; }
    int getMaskSize() { return _num_indices;} 	// guaranteed by canon
    int isValid() { return _is_valid; }

    // This should really never be used except by the copy ctor.  The
    // instrument is only needed to canoncalize the INSTRUMENT frame;
    // we probably don't really need to store it but just in case...

    const char *getInstId() const { return _inst_id; }

    ///////////////////////////////////////////////////////////////////
    // Compare two PigCSReference objects.  Return 1 if objects are equal
    // 0 if not.   Note that if one Index set is shorter than the other, it
    // is filled with 0's.
    // (e.g. (3,1,5) compared to (3,1), the second is filled to (3,1,0))
    // Also, a null or empty solutionID for "this" object only is treated
    // as a wildcard, matching anything.  Null or empty solutionID for the
    // "other" object is treated as identity.
    ///////////////////////////////////////////////////////////////////
	
    int isEqual(PigCSReference *csRef);

    const char *const getModelName() { return "CSReference"; }

};

#endif
