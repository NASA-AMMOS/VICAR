////////////////////////////////////////////////////////////////////////
// PigSolutionManager
//
// This is an abstract base class for things that have Solutions (formerly
// called Opinions), including Rover State and pointing correction
// information.  The info generally comes from XML files, although it
// could also come from labels.
//
// Solutions are managed based on their "priority", which is an ordered
// list of solution ID's.  If multiple solutions satisfy the lookup key
// (e.g. coord frame name and index (RMC) for rover state, image ID info
// for pointing correction), the highest priority solution is the one used.
//
// Internally, things are managed using PigSolutionItem subclasses (no longer
// by DOM structures!), so labels or XML input are converted to the appropriate
// PigSolutionItem subclass for each subclass.
//
// Before use, the PSM object must be set up, where you provide XML files
// and/or labels to ingest.  Generally, all setup should be done before use,
// although this is not a requirement (retrieving information simply reflects
// the state of setup at the time the request is issued).
//
// The PSM object maintains all information in memory; the setup files are
// not used after they are read.
//
// It's quite likely that derived classes could be singletons, but this
// base class is not.
//
// !!!!TBD:  Need to look at the "mission" tag on the basename and ignore
// !!!!TBD:  any entries which do not belong to this mission.  The Solution
// !!!!TBD:  Manager is supposed to be a singleton per mission and solution
// !!!!TBD:  type, so it should ignore other missions' data.  THIS IS NOT
// !!!!TBD:  CURRENTLY DONE!!  Providing multiple missions will really
// !!!!TBD:  confuse things at the moment.
////////////////////////////////////////////////////////////////////////
#ifndef PIGSOLUTIONMANAGER_H
#define PIGSOLUTIONMANAGER_H

#include "PigModelBase.h"
#include "PigXerces.h"
#include "PigMission.h"

#include <vector>
using namespace std;

class PigSurfaceModelParams;

// Intended to be subclassed - used only as the contents of the Lists
// Each subclass needs a #define for the type

#define PIG_SOLUTION_TYPE_COORD 1
#define PIG_SOLUTION_TYPE_POINTCORR 2
#define PIG_SOLUTION_TYPE_SURFACE 3

class PigSolutionItem {
  public:
    virtual const char *getPriority() = 0;
    virtual int getSolutionItemType() = 0;
};

typedef vector<PigSolutionItem *> PigSolutionList;

class PigSolutionManager : public PigModelBase {

  protected:
    vector<char *> _priorityList;
    vector<PigSolutionList> _prioritySolutionList; // indexed by pri (hi last)
    vector<PigSolutionList> _prioritySurfaceModelList; // ditto

    PigSolutionList _leftoverSolutionList;

    PigSolutionList _leftoverSurfaceModelList;

    char *_basename;
    PigMission *_mission;

    // Find the given priority and return its index.  Returns -1 if the
    // priority was not found.

    virtual int findPriority(const char *priority);

    // Print solutions for a single priority.

    virtual void printSolutions(PigSolutionList *list);

    // Print surface_models for a single priority.  There should be only one
    // surface_model defined per priority.  If more than one found, write out
    // warning message. 

    virtual void printSurfaceModels(PigSolutionList *list);

    // Print interesting info for a single solution.  This must be
    // overridden by subclasses.

    virtual void printSolution(PigSolutionItem *solution) = 0;

    // Print interesting info for a single surface model.  This need not be
    // overridden by subclasses.

    virtual void printSurfaceModel(PigSolutionItem *item);

    // Convert XML to appropriate PigSolutionItem type.  Must be implmented
    // by subclasses.  Create and return the object (caller must free)

    virtual PigSolutionItem *xmlToItem(DOMElement *solution) = 0;

    // Convert XML to PigSolutionItem for surface model.  Base class impl
    // should be fine.  Create and return the object (caller must free)

    virtual PigSolutionItem *xmlToItemSM(DOMElement *solution);

    // Hook for doing special things after a solution is added.  For
    // example, this could add extra constant coord system definitions
    // into the database.

    virtual void addSpecialSolutions(PigSolutionItem *item) { }

  public:

    // Basename is just an identifier for prints
    PigSolutionManager(PigMission *m, char *basename);
    virtual ~PigSolutionManager();

    // Read in the given file.  This is the primary setup routine that
    // external users should call.  Each subsequent file added gets a higher
    // priority.  The exclude_telemetry flag, if true, will cause the special
    // solution name "telemetry" to not be added to the priority list
    // (since it should normally be only at the beginning of the list).  See
    // addPriorities().

    virtual void readSolutionFile(char *filename, int exclude_telemetry);

    // Handle any subclass-specific elements here.  Subclasses should override
    // this to handle anything other than <priority> and <solution> tags that
    // they care about (e.g. <alias> tags in an RMC file).

    virtual void processExtraElements(DOMDocument *doc) { }

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

    virtual void addPriorities(DOMElement *parent, int exclude_telemetry);

    // Add a single priority to the end of the list (highest priority).
    // If the name already exists, the addition is ignored, so the original
    // priority is retained.

    virtual void addPriority(const char *priority);

    // Print the priorities (in order, highest last) as an Info message.

    virtual void printPriorities();

    // Add any <solution> elements found within the given element into
    // the database of solutions.  Order shouldn't matter, but they are
    // appended to the end.

    virtual void addSolutions(DOMElement *parent);

    // Add any <surface_model> elements found within the given element into
    // the database of surface_models.  Order shouldn't matter, but they are
    // appended to the end.

    virtual void addSurfaceModels(DOMElement *parent);

    // Add a single given <solution> element into the database of solutions.
    // Order shouldn't matter, but they are appended to the end.

    virtual void addSolution(DOMElement *solution);

    // Add a single item to the database of solutions.
    // We take over memory management of the item.

    virtual void addSolution(PigSolutionItem *item);

    // Add a single given <surface_model> element into the database of surface_models.
    // Order shouldn't matter, but they are appended to the end.

    virtual void addSurfaceModel(DOMElement *solution);

    // Print info about the loaded solutions as an Info message.

    virtual void printSolutions();

    // Print info about the loaded surface_models as an Info message.

    virtual void printSurfaceModels();

    // Get a surface model

    virtual PigSurfaceModelParams *getSurfaceModel(const char *solution_id);

    PigMission *getMission() { return _mission; }

    // Utility routine to get last item in list into a C string.
    // The string is dynamically allocated and must be deleted by the caller!

    virtual char *getHighestPriority();

    // Get the solution ID from a <solution> element into a C string.
    // The string is dynamically allocated and must be deleted by
    // the caller!  Returns NULL if the ID is not available (which
    // should not happen)

//!!!!????  static char *getSolutionId(DOMElement *solution);
};

#endif

