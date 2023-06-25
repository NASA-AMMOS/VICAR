////////////////////////////////////////////////////////////////////////
// PigCSDefinition
//
// Contains all information necessary to both identify, and define, a
// Coordinate System.  This distinguishes it from PigCSReference, in that
// the latter contains only identifying information.  This class also
// contains the identification of the reference system, as well as the
// transform between them.
////////////////////////////////////////////////////////////////////////

#ifndef PIGCSDEFINITION_H
#define PIGCSDEFINITION_H

#include "PigCSReference.h"
#include "PigSolutionManager.h"
#include "lbl_coordinate.h"
#include <string.h>

class PigCSDefinition : public PigSolutionItem {

  protected:
    PigCSReference *_identity;		// name of this coord system
    PigCSReference *_ref_system;	// name of the reference system

    PigVector _offset;			// transform to the reference system
    PigQuaternion _quat;
    int _values_valid;			// true if both offset and quat present

    void lbl_init(PigMission *m, LblCoordinate_typ *lblCoordinate,
							const char *inst);

  public:

    // Main constructor

    PigCSDefinition(PigMission *m, PigCSReference *ident, PigCSReference *ref,
		PigVector offset, PigQuaternion quat);

    // Construct from a label.

    PigCSDefinition(PigMission *m, LblCoordinate_typ *lblCoordinate);
    PigCSDefinition(PigMission *m, LblCoordinate_typ *lblCoordinate,
							const char *inst);

    // Construct from XML

    PigCSDefinition(PigMission *m, DOMElement *node);

    // Copy constructor

    PigCSDefinition(const PigCSDefinition &from);

    ~PigCSDefinition();

    ////////////////////////////////////////////////////////////////////

    PigCSReference *getIdentity() const { return _identity; }

    PigCSReference *getReference() const { return _ref_system; }

    void setOffset(PigVector offset) { _offset = offset; }
    PigVector getOffset() const { return _offset; }

    void setQuaternion(PigQuaternion quat) { _quat = quat; }
    PigQuaternion getQuaternion() const { return _quat; }

    int isValid() { return _identity && _identity->isValid() &&
			   _ref_system && _ref_system->isValid() &&
			   _values_valid; }

    const char *const getModelName() { return "CSDefinition"; }

    // PigSolutionItem methods
    virtual const char *getPriority() { return _identity->getSolutionId(); }
    virtual int getSolutionItemType() { return PIG_SOLUTION_TYPE_COORD; }

};

#endif
