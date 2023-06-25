// perf_object.h - Performer Object Encapsulation for the Grape Renderer
//
// The PerfObj class is a simple encapsulation of a Performer object with a
// GRAPE wrapper.  Fundamentally, it is simply a GRAPE object with a filename
// which references a Performer object file.  The file is never accessed
// until rendering time when the Performer renderer retrieves the filename and
// reads in the model file.
//
// see also PerfObj man page

#ifndef _PERFOBJECT_H_
#define _PERFOBJECT_H_

#ifdef _USE_PERFORMER_	// must include a -D_USE_PERFORMER_ in your Makefile

#include "grape/object.h"
#include "grape/light.h"

#include "image/image.h"
#include "image/datatypes.h"
#include "image/filetypes.h"

#include <Performer/pf/pfNode.h>
#include <Performer/pf/pfDCS.h>

#include <Performer/pfutil.h>
#include <Performer/pfdu.h>

class PerfObj : public Obj
{

  protected:

    char	*version;
	char	*file_type;		// needed?
	char	*fname;			// filename of actual geometry
	char	*group_name;	// name of subtree - not implemented

	pfDCS    *dcs;			// transformations (if any) applied here

  public:

	// initialize PerfObj from a file
	int		parse_in(Dataport *fp);
	// write object info out to a file/files
	int		parse_out(Dataport *fp, int expand=NULL); // expand not used

	// initialize a pfNode w/ a filename and attach to a pfDCS
	int		set_file(char *filename=NULL);

	// load Grape ZMatrix into Performer pfDCS
	int		apply_transforms(ObjNode *ON);

	// return a pfDCS pointer
	pfDCS   *get_pfDCS_ptr();

	// get/set_changed not implemented yet
	void	set_changed() { changed=CHANGE_COUNTER; }
	int		get_changed() { return(0); }

	int		get_type(void)  { return(PERF_V1); }

	//Constructors
	
	PerfObj(void)
	{
	    version = NULL;
		file_type = NULL;
		group_name = NULL;
		fname = NULL;
		dcs = NULL;
	}

	// initialize w/ a filename, but do not load the geometry yet
	PerfObj(char *filename)
	{
	    version = NULL;
		file_type = NULL;
		group_name = NULL;
		dcs = NULL;

		if(fname)free(fname);
		fname = strdup(filename);
	}


	~PerfObj(void)    //Destructor
	{
	    if(dcs)pfDelete(dcs);
		if(version)free(version);
		if(file_type)free(file_type);
		if(group_name)free(group_name);
		if(fname)free(fname);
	}
};

// convenience functions for converting matrix formats from Performer to
// GRAPE and vise-versa
void convert_pfMat_to_ZMat(pfMatrix *pfmat, ZMatrix mat);

void convert_ZMat_to_pfMat(ZMatrix mat, pfMatrix *pfmat);

#endif	// _USE_PERFORMER_

#endif	// _PERFOBJECT_H_
