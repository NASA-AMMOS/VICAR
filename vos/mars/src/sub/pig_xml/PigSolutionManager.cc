////////////////////////////////////////////////////////////////////////
// PigSolutionManager
//
// This base class manages Solutions, including info from XML files and
// from labels.
////////////////////////////////////////////////////////////////////////

#include "PigSolutionManager.h"
#include "PigXerces.h"
#include "PigSurfaceModelParams.h"
#include <string.h>
#include <stdlib.h>

#define PRIO_INC 5	/* how much to expand the table each time */

////////////////////////////////////////////////////////////////////////
// Constructor
//
// The base name is the name of the root element to use, and is also used
// for info prints.  It really doesn't matter since the element is internal,
// but it's nice to have it match the name of the root element of the source
// XML documents.
////////////////////////////////////////////////////////////////////////

PigSolutionManager::PigSolutionManager(PigMission *m, char *basename)
		: PigModelBase()
{
    PigXerces::initialize();

    _mission = m;
    _basename = strdup(basename);
}

PigSolutionManager::~PigSolutionManager()
{
    // the vectors all get automatically freed

    delete _basename;

    PigXerces::close();
}

////////////////////////////////////////////////////////////////////////
// Read in the given file.  This is the primary setup routine that
// external users should call.  Each subsequent file added gets a higher
// priority.  The exclude_telemetry flag, if true, will cause the special
// solution name "telemetry" to not be added to the priority list
// (since it should normally be only at the beginning of the list).  See
// addPriorities().
////////////////////////////////////////////////////////////////////////

void PigSolutionManager::readSolutionFile(char *filename, int exclude_telemetry)
{

printf("reading RVF file %s\n", filename);	//!!!!
    DOMDocument *doc = PigXerces::parseFile(filename);
    if (doc == NULL)
	return;		// nothing to do...

    DOMElement *elem = doc->getDocumentElement();
    addPriorities(elem, exclude_telemetry);
    addSolutions(elem);

    addSurfaceModels(elem);

    processExtraElements(doc);

    doc->release();		// done with it...
}

////////////////////////////////////////////////////////////////////////
// Add any <priority> elements found within the given element into the
// priority list.  Priorities are appended to the end of the list, so
// anything in this document has higher priority than any existing ones.
// The flag, if true, will remove the special name "telemetry" and not
// add it to the list (since it should normally be only at the beginning).
//
// Priority elements look like this:
// <priority>
//   <entry id="low_prio"/>
//   <entry id="mid_prio"/>
//   <entry id="high_prio"/>
// </priority>
// where <entry> has 0-n occurrences and id is required.  There should be
// just one <priority> tag but multiples are allowed (each subsequent
// tag has higher priority than its predecessors).
////////////////////////////////////////////////////////////////////////

void PigSolutionManager::addPriorities(DOMElement *parent,int exclude_telemetry)
{
    if (parent == NULL)
	return;

    DOMNodeList *priorities = PigXerces::getElementsByTagName(parent,
								"priority");
    if (priorities == NULL) {
	printWarning("No <priority> entries found in file");
	return;
    }
    // There really should be only one <priority> element, but just in case...
    for (int prio = 0; prio < priorities->getLength(); prio++) {

	DOMElement *priority = PigXerces::nextElement(priorities, prio);

	DOMNodeList *entries = PigXerces::getElementsByTagName(priority,
								"entry");
	int new_entries = 0;
	if (entries != NULL) new_entries = entries->getLength();
	if (new_entries != 0) {

	    // Add the entries to the list

	    for (int i=0; i < new_entries; i++) {
		DOMElement *entry = PigXerces::nextElement(entries, i);
		char *id = PigXerces::getAttribute(entry, "solution_id");
		if ( ! (exclude_telemetry && strcasecmp(id,"telemetry")==0))
		    addPriority(id);
		XMLString::release(&id);
	    }
	}
    }
}

////////////////////////////////////////////////////////////////////////
// Add a single priority to the end of the list (highest priority).
// If a name already exists, the addition is ignored, so the original
// priority is retained.
//
// This is implemented rather inefficiently but it is not expected that
// this routine will be called all that often.
//
// Creates its own copy of the supplied string.
////////////////////////////////////////////////////////////////////////

void PigSolutionManager::addPriority(const char *priority)
{
    int i;

    // Check for duplicates

    i = findPriority(priority);

    if (i >= 0)
	return;			// Duplicate, so ignore it

    // Add the entry to the end of the list

    _priorityList.push_back(strdup(priority));
    _prioritySolutionList.push_back(PigSolutionList());
    _prioritySurfaceModelList.push_back(PigSolutionList());
    int curr_pri = _priorityList.size() - 1;

    // The last thing to do is to check the leftover solution table
    // to see if any match this new priority...

    int size = _leftoverSolutionList.size();
    for (i=0; i < size; i++) {
	PigSolutionItem *item = _leftoverSolutionList[i];
	if (strcasecmp(item->getPriority(), priority) == 0) {
	    _prioritySolutionList[curr_pri].push_back(item);
	    _leftoverSolutionList.erase(_leftoverSolutionList.begin()+i);
	    i--;
	}
    }

    size = _leftoverSurfaceModelList.size();
    for (i=0; i < size; i++) {
	PigSolutionItem *item = _leftoverSurfaceModelList[i];
	if (strcasecmp(item->getPriority(), priority) == 0) {
	    _prioritySurfaceModelList[curr_pri].push_back(item);
	    _leftoverSurfaceModelList.erase(
					_leftoverSurfaceModelList.begin()+i);
	    i--;
	}
    }
}

////////////////////////////////////////////////////////////////////////
// Find the given priority and return its index.  Returns -1 if the
// priority was not found.
////////////////////////////////////////////////////////////////////////

int PigSolutionManager::findPriority(const char *priority)
{
    const char *p = priority;
    if (p == NULL)
        p = "telemetry";
    for (int i=0; i < _priorityList.size(); i++) {
	if (strcasecmp(_priorityList[i],p) == 0)
	    return i;
    }
    return -1;
}

////////////////////////////////////////////////////////////////////////
// Print the priorities (in order, highest last) as an Info message.
////////////////////////////////////////////////////////////////////////

void PigSolutionManager::printPriorities()
{
    char msg[256];

    int size = _priorityList.size();
    if (size != 0) {
	sprintf(msg, "ID Priority List for %s (lowest first):", _basename);
	printInfo(msg);
	for (int i=0; i<size; i++) {
	    sprintf(msg, "  %s", _priorityList[i]);
	    printInfo(msg);
	}
    }
    else {
	sprintf(msg, "ID Priority List for %s is empty", _basename);
	printInfo(msg);
    }
}

////////////////////////////////////////////////////////////////////////
// Add any <solution> elements found within the given element into
// the database of solutions.  Order shouldn't matter, but they are
// appended to the end.
////////////////////////////////////////////////////////////////////////

void PigSolutionManager::addSolutions(DOMElement *parent)
{
    if (parent == NULL)
	return;
 
    DOMNodeList *solutions = PigXerces::getElementsByTagName(parent,"solution");

    if (solutions == NULL) {
	printWarning("No <solution> elements found in file");
	return;
    }

    // Add each solution to the internal databases

    for (int i = 0; i < solutions->getLength(); i++) {

	DOMElement *solution = PigXerces::nextElement(solutions, i);
	if (solution != NULL) {
	    addSolution(solution);
	}
    }
}

////////////////////////////////////////////////////////////////////////
// Add a single given <solution> element into the database of solutions.
// Order shouldn't matter, but they are appended to the end.
////////////////////////////////////////////////////////////////////////

void PigSolutionManager::addSolution(DOMElement *solution)
{
    // Convert from XML to item

    PigSolutionItem *item = xmlToItem(solution);

    addSolution(item);
}

////////////////////////////////////////////////////////////////////////
// Add a single item to the database of solutions.
// We take over memory management of the item
////////////////////////////////////////////////////////////////////////

void PigSolutionManager::addSolution(PigSolutionItem *item)
{
    int j = findPriority(item->getPriority());
    if (j >= 0)		// matching solution
	_prioritySolutionList[j].push_back(item);
    else
	_leftoverSolutionList.push_back(item);

    addSpecialSolutions(item);
}

////////////////////////////////////////////////////////////////////////
// Add any <surface_model> elements found within the given element into
// the database of surface_models.  Order shouldn't matter, but they are
// appended to the end.  There should be at most one <surface_model> defined
// per solution_id
////////////////////////////////////////////////////////////////////////

void PigSolutionManager::addSurfaceModels(DOMElement *parent)
{
    if (parent == NULL)
	return;
 
    DOMNodeList *surface_models = PigXerces::getElementsByTagName(parent,
							"surface_model");

    if (surface_models == NULL)
	return;				// no surface models is not an error

    // Add each surface_model to the internal databases

    for (int i = 0; i < surface_models->getLength(); i++) {

	DOMElement *surface_model = PigXerces::nextElement(surface_models, i);
	if (surface_model != NULL)
	    addSurfaceModel(surface_model);
    }
}

////////////////////////////////////////////////////////////////////////
// Add a single given <surface_model> element into the database of
// surface_models.  Order shouldn't matter, but they are appended to the end.
////////////////////////////////////////////////////////////////////////

void PigSolutionManager::addSurfaceModel(DOMElement *surface_model)
{
    // Convert from XML to item

    PigSolutionItem *item = xmlToItemSM(surface_model);

    int j = findPriority(item->getPriority());
    if (j >= 0)		// matching surface_model
	_prioritySurfaceModelList[j].push_back(item);
    else
	_leftoverSurfaceModelList.push_back(item);
}

////////////////////////////////////////////////////////////////////////
// Get a surface model
////////////////////////////////////////////////////////////////////////
PigSurfaceModelParams *PigSolutionManager::getSurfaceModel(
						const char *solution_id)
{
    // Find given solution id
    int cnt = findPriority(solution_id);

    PigSolutionItem *sm;
    if (cnt > -1) {			// priority has been found
	sm = _prioritySurfaceModelList[cnt].front();
    } else if (solution_id == NULL) {	// return highest priority solution
	sm = _prioritySurfaceModelList.back().front();
    } else {
	return NULL;
    }
    if (sm->getSolutionItemType() != PIG_SOLUTION_TYPE_SURFACE) {
	char msg[1024];
	sprintf(msg, "Internal error: unexpected item type '%d' in PigSolutionManager::getSurfaceModel", sm->getSolutionItemType());
	printError(msg);
	return NULL;
    }
    return (PigSurfaceModelParams *)sm;
}

////////////////////////////////////////////////////////////////////////
// Print info about the loaded solutions as an Info message.  Solutions are
// sorted by priority.
////////////////////////////////////////////////////////////////////////

void PigSolutionManager::printSolutions()
{
    char msg[256];

    for (int i=0; i < _priorityList.size(); i++) {
	sprintf(msg, "Solutions for priority %s:", _priorityList[i]);
	printInfo(msg);

	printSolutions(&_prioritySolutionList[i]);
    }

    printInfo("Solutions with unassigned priorities:");
    printSolutions(&_leftoverSolutionList);
}

////////////////////////////////////////////////////////////////////////
// Print solutions for a single priority.
////////////////////////////////////////////////////////////////////////

void PigSolutionManager::printSolutions(PigSolutionList *list)
{
    char msg[256];

    int size = list->size();
    
    if (size == 0)
	printInfo("  None");
    else {
	for (int i=0; i<size; i++) {
	    printSolution((*list)[i]);
	}
    }
}
////////////////////////////////////////////////////////////////////////
// Print info about the loaded surface_models as an Info message.
// Surface_Models are sorted by priority.
////////////////////////////////////////////////////////////////////////

void PigSolutionManager::printSurfaceModels()
{
    char msg[256];

    for (int i=0; i < _priorityList.size(); i++) {
	sprintf(msg, "Surface_Models for priority %s:", _priorityList[i]);
	printInfo(msg);

	printSurfaceModels(&_prioritySurfaceModelList[i]);
    }

    printInfo("Surface_Models with unassigned priorities:");
    printSurfaceModels(&_leftoverSurfaceModelList);
}

////////////////////////////////////////////////////////////////////////
// Print surface_models for a single priority.  There should be only one
// surface_model defined per priority.  If more than one found, write out
// warning message. 
////////////////////////////////////////////////////////////////////////

void PigSolutionManager::printSurfaceModels(PigSolutionList *list)
{
    char msg[256];
    
    int size = list->size();

    if (size == 0)
	printInfo("  None");
    else {
        if (size > 1)
	    printWarning("Warning: More than one surface model defined for a given priority");
	for (int i=0; i<size; i++) {
	    printSurfaceModel((*list)[i]);
	}
    }
}

////////////////////////////////////////////////////////////////////////
// Print out a single surface model
////////////////////////////////////////////////////////////////////////

void PigSolutionManager::printSurfaceModel(PigSolutionItem *item)
{
    char msg[1024];
    if (item->getSolutionItemType() != PIG_SOLUTION_TYPE_SURFACE) {
	sprintf(msg, "Internal error: unexpected item type '%d' in PigSolutionManager::printSurfaceModel", item->getSolutionItemType());
	printError(msg);
    }
    PigSurfaceModelParams *smp = (PigSurfaceModelParams *)item;
    sprintf(msg, "  type=%s, solution_id=%s", smp->getType(),
						smp->getSolutionID());
    printInfo(msg);
}

////////////////////////////////////////////////////////////////////////
// Convert XML to PigSolutionItem for surface model.  Base class impl
// should be fine.  Create and return the object (caller must free)
////////////////////////////////////////////////////////////////////////

PigSolutionItem *PigSolutionManager::xmlToItemSM(DOMElement *solution)
{
    return new PigSurfaceModelParams(_mission, solution);
}


//!!!!???? ////////////////////////////////////////////////////////////////////////
//!!!!???? // Utility routine to get the solution ID from a <solution> element into
//!!!!???? // a C string.  The string is dynamically allocated and must be deleted
//!!!!???? // by the caller!  Returns NULL if the ID is not available (which should
//!!!!???? // not happen)
//!!!!???? ////////////////////////////////////////////////////////////////////////
//!!!!???? char *PigSolutionManager::getSolutionId(DOMElement *solution)
//!!!!???? {
//!!!!????     return PigXerces::getAttributeCstr(solution, "solution_id");
//!!!!???? }

///////////////////////////////////////////////////////////////////////
// Utility routine to get last item in <priority> list into a C string.
// The string is dynamically allocated and must be deleted by the caller!
///////////////////////////////////////////////////////////////////////
char *PigSolutionManager::getHighestPriority()
{
    return strdup(_priorityList.back());
}

