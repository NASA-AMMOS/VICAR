// forest.C 1.3 03/03/26 15:39:29
/** \file
 ** Forest class implementation
 **/

#include <string.h>
#include "grape/sfcmodel.h"
#include "forest.h"
#include "patch.h"

/// Read forest from file, optionally load object data
int Forest::parse_in(Dataport *fp, int expand)
{
	if (!parse_start(fp))		// generic part
		return FALSE;

	// allocate and parse children - must be Patches with SfcModels
	for (int i=0; i<num_children; i++) {
		Patch *p = new Patch;
		if (!p->parse_in(fp, expand)) {
			num_children = i;
			return FALSE;
		}
		children[i] = p;
	}

	return TRUE;
}

/// Search forest for patch matching given identifier string.
// Returns child number, or -1 if not found
int Forest::find_patch(const char *ident)
{
	for (int i=0; i<num_children; i++) {
		Patch *p = get_patch(i);
		if (!strcmp(p->get_name(), ident))
			return i;	// found a match
	}
	return -1;
}
