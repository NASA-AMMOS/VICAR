////////////////////////////////////////////////////////////////////////
// PigCSDefinition
//
// Contains all information necessary to both identify, and define, a
// Coordinate System.  This distinguishes it from PigCSReference, in that
// the latter contains only identifying information.  This class also
// contains the identification of the reference system, as well as the
// transform between them.
////////////////////////////////////////////////////////////////////////

#include "PigCSDefinition.h"
#include "return_status.h"
#include "PigRoverStateManager.h"
#include "PigMission.h"

////////////////////////////////////////////////////////////////////////
// Main constructor
////////////////////////////////////////////////////////////////////////

PigCSDefinition::PigCSDefinition(PigMission *m, PigCSReference *ident,
		PigCSReference *ref, PigVector offset, PigQuaternion quat)
{
    _identity = new PigCSReference(*ident);
    _ref_system = NULL;
    if (ref != NULL)
        _ref_system = new PigCSReference(*ref);

    _offset = offset;
    _quat = quat;

}

////////////////////////////////////////////////////////////////////////
// Construct from a label.  Optional instrument just to set it in the ref.
////////////////////////////////////////////////////////////////////////

PigCSDefinition::PigCSDefinition(PigMission *m,
				LblCoordinate_typ *lblCoordinate)
{ lbl_init(m, lblCoordinate, NULL); }

PigCSDefinition::PigCSDefinition(PigMission *m,
				LblCoordinate_typ *lblCoordinate,
				const char *inst)
{ lbl_init(m, lblCoordinate, inst); }

void PigCSDefinition::lbl_init(PigMission *m,
				LblCoordinate_typ *lblCoordinate,
				const char *inst)
{
    char *name = NULL;
    if (lblCoordinate->CoordinateSystemName.Valid)
        name = lblCoordinate->CoordinateSystemName.Value;

    char *solution = NULL;
    if (lblCoordinate->SolutionId.Valid)
        solution = lblCoordinate->SolutionId.Value;

    int index_size = 0;
    int index[PIG_MAX_CS_INDEX];
    for (int cnt = 0; cnt < PIG_MAX_CS_INDEX; cnt++) {
        if (lblCoordinate->CoordinateSystemIndex[cnt].Valid) {
	    index[cnt] = lblCoordinate->CoordinateSystemIndex[cnt].Value;
	    index_size++;
	}
	else		// once we run out, don't add any more...
            break;	// i.e. don't allow holes
    }

    // INSTRUMENT may not be valid in a label
    _identity = new PigCSReference(m, name, solution, index, index_size, inst);

    // Add the transform elements from the label...

    _values_valid = TRUE;

    if (lblCoordinate->OriginOffsetVector.Valid == LBL_VALID)
	_offset.setXYZ(lblCoordinate->OriginOffsetVector.Value);
    else
	_values_valid = FALSE;			// UNK, NA, etc
    if (lblCoordinate->OriginRotationQuaternion.Valid == LBL_VALID)
	_quat.setComponents(lblCoordinate->OriginRotationQuaternion.Value);
    else
	_values_valid = FALSE;			// UNK, NA, etc

    // Now add the reference system...

    name = NULL;
    if (lblCoordinate->ReferenceCoordSystemName.Valid)
	name = strdup(lblCoordinate->ReferenceCoordSystemName.Value);

    solution = NULL;
    if (lblCoordinate->ReferenceCoordSystemSolnId.Valid)
        solution = lblCoordinate->ReferenceCoordSystemSolnId.Value;

    index_size = 0;
    for (int cnt = 0; cnt < PIG_MAX_CS_INDEX; cnt++) {
        if (lblCoordinate->ReferenceCoordSystemIndex[cnt].Valid) {
	    index[cnt] = lblCoordinate->ReferenceCoordSystemIndex[cnt].Value;
	    index_size++;
	}
	else		// once we run out, don't add any more...
            break;	// i.e. don't allow holes
    }
    _ref_system = new PigCSReference(m, name, solution, index, index_size,NULL);

}

////////////////////////////////////////////////////////////////////////
// Construct from XML
////////////////////////////////////////////////////////////////////////

PigCSDefinition::PigCSDefinition(PigMission *m, DOMElement *node)
{
    // Normally it's in the solution node itself.  But sometimes it's in
    // an "rmc" child.  So if such a child exists, use it.

    DOMElement *frame = PigXerces::getOneElement(node, "rmc");
    if (frame != NULL)
	_identity = new PigCSReference(m, frame, NULL);
    else				// normal case
        _identity = new PigCSReference(m, node, NULL);
    frame = PigXerces::getOneElement(node, "reference_frame");
    if (frame == NULL) {
	// try a child reference_rmc within solution_metadata
	frame = PigXerces::getOneElement(node, "solution_metadata");
	if (frame != NULL) {
	    frame = PigXerces::getOneElement(frame, "reference_rmc");
	}
    }
    _ref_system = NULL;
    if (frame != NULL)
        _ref_system = new PigCSReference(m, frame, NULL);

    _offset = PigRoverStateManager::getSolutionVector(node);
    _quat = PigRoverStateManager::getSolutionQuaternion(node);
}

////////////////////////////////////////////////////////////////////////
// Copy constructor
////////////////////////////////////////////////////////////////////////

PigCSDefinition::PigCSDefinition(const PigCSDefinition &from)
{
    _identity = new PigCSReference(*(from.getIdentity()));
    _ref_system = NULL;
    if (from.getReference() != NULL)
        _ref_system = new PigCSReference(*(from.getReference()));
    _offset = from.getOffset();
    _quat = from.getQuaternion();

}


PigCSDefinition::~PigCSDefinition()
{
    if (_identity)
	delete _identity;
    if (_ref_system)
	delete _ref_system;
}

