////////////////////////////////////////////////////////////////////////
// PigMission
//
// Base class for all mission-specific interface functions.  These functions
// are used whenever mission-independent code needs to create a mission-
// specific object.  The static function getMissionObject() returns an
// instance of this class specific for the given mission.
//
// The interface functions are all gathered here to make it easier to
// eventually go to a dynamically-loaded shared library for all project
// support, and to minimize the mission-specific items that must be in
// multimission code.
//
// Note that for missions that have multiple spacecrafts(hosts) per mission,
// such as MER, the mission name is the string of the form: "mission:host_id".
// Also note, that usually mission objects contain no data, just virtual 
// functions to create other objects.  This is not always true, for missions
// with multiple spacecrafts as described above, we store mission_name and
// host_id.
//
////////////////////////////////////////////////////////////////////////

#include "PigMission.h"

#include "PigM20.h"
#include "PigNSYT.h"
#include "PigMSL.h"
#include "PigPHX.h"
#include "PigMER.h"
#include "PigFIDO.h"
#include "PigM01.h"
// #include "PigM98.h"
#include "PigMPF.h"
#include "PigGenericImage.h"
#include "PigColdarm.h"
#include "PigPsyche.h"

#include "PigUtilities.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "PigCSReference.h"
#include "PigCSDefinition.h"

#include "PigSurfaceInfinity.h"
#include "PigSurfaceSphere2.h"
#include "PigSurfaceSphere1.h"
#include "PigSurfacePlane.h"
// Mesh support activated depending on OS arch - 
// Pig supports .obj mesh for surface model. The ray tracer used internally to
// intersect the mesh with viewing pixels is handled by the Intel Embree lib, 
// which is only available on 64-bit OSes. The mesh support is activated 
// accordingly. See imake_util.tmp and imake.config
#if USES_PIG_MESH
#include "PigSurfaceMesh.h"  
#endif

#include "zvproto.h" 

#include "PigCAHV.h"		// only for the utility routines at the end
#include "PigCAHVOR.h"		// only for the utility routines at the end
#include "PigCAHVORE.h"		// only for the utility routines at the end

PigMission *PigMission::_mpf = NULL;
// PigMission *PigMission::_m98 = NULL;
PigMission *PigMission::_generic_image = NULL;
PigMission *PigMission::_m01 = NULL;
PigMission *PigMission::_fido = NULL;
PigMission *PigMission::_mer[10];
PigMission *PigMission::_phx[10];
PigMission *PigMission::_msl[10];
PigMission *PigMission::_nsyt[10];
PigMission *PigMission::_m20[10];
PigMission *PigMission::_coldarm[10];
PigMission *PigMission::_psyche[10];
PigMission PigMission::_defaultObject;

int PigMission::_mer_host_count = 0;
int PigMission::_phx_host_count = 0;
int PigMission::_msl_host_count = 0;
int PigMission::_nsyt_host_count = 0;
int PigMission::_m20_host_count = 0;
int PigMission::_coldarm_host_count = 0;
int PigMission::_psyche_host_count = 0;

////////////////////////////////////////////////////////////////////////
// Extract mission from a file and return the object.  The file is
// returned open (on success).  Note that this is the only place
// that a mission name is derived in the PIG system.  Some missions,
// such as MER, have more than one spacecraft(host), and this is indicated
// by making the string of the form "mission:host_id" for those missions.
// Also note that the mission names match what is returned by zgetproj()
////////////////////////////////////////////////////////////////////////

PigMission *PigMission::getMissionObject(const char *filename, int *unit)
{
    int status;
    char mission[33];
    char host_id[33];

    strcpy(mission, "");
    strcpy(host_id, "");
    status = PigGetMission(filename, unit, mission, host_id);
    if (status != 0)
        return NULL;

    // ignore status, invalid mission is picked up in the below call.

    // The code below is required for missions that have more than one
    // spacecraft(host) - that extra is often the testbed.
    if((strcmp(mission, "MER") == 0) || (strcmp(mission,"MSL") == 0)
		|| (strcmp(mission, "NSYT")==0) || (strcmp(mission,"M20") == 0)
		|| (strcmp(mission, "COLDARM")==0) || (strcmp(mission,"PSYCHE")==0))
        sprintf(mission, "%s:%s", mission, host_id);

    // Some old pre-ATLO PHX labels used "PHX" for the host name; make
    // those into FM for compatibility.  That check can be removed later.

    if (strcmp(mission, "PHX") == 0) {
	if (strcmp(host_id, "PHX") == 0)
	    sprintf(mission, "%s:FM", mission);
	else
	    sprintf(mission, "%s:%s", mission, host_id);
    }
    return getMissionObject(mission);
}

////////////////////////////////////////////////////////////////////////
// Extract mission from a PigFile object and return the object.
////////////////////////////////////////////////////////////////////////

PigMission *PigMission::getMissionObject(PigFileModel *file)
{
    return getMissionObject(file->getMissionName());
}

////////////////////////////////////////////////////////////////////////
// The real work.  Compare the mission to known names and return the
// proper subclass.  We compare the most recent missions first, for
// slight efficiency gains.
////////////////////////////////////////////////////////////////////////

PigMission *PigMission::getMissionObject(const char *mission)
{

    if(mission != NULL && (strncasecmp(mission, "M20", 3) == 0)) {
	const char *host_id;

	// For  M20 mission name is of the form: "mission:host_id"
	// separate them into two separate strings.
	host_id = strchr(mission, ':');

        for(int cnt = 0; cnt < _m20_host_count; cnt++) {
	  if((_m20[cnt] != NULL) &&
	     (strcmp(((PigM20 *)_m20[cnt])->getHostID(), host_id+1) == 0))
	      // we got a match
	      return _m20[cnt];	    
	}
  	
	// No match has been found, so create new M20 mission object
	_m20[_m20_host_count] = new PigM20(mission, host_id+1);
	
	if(_m20[_m20_host_count] == NULL) {
	    printStaticMsg("Error allocating M20 Mission object!", 
			   PigMsgFatal);
	    return &_defaultObject;
	}

	//increment the count
	_m20_host_count++;

        return _m20[_m20_host_count-1];

    }

    if(mission != NULL && (strncasecmp(mission, "MSL", 3) == 0)) {
	const char *host_id;

	// For  MSL mission name is of the form: "mission:host_id"
	// separate them into two separate strings.
	host_id = strchr(mission, ':');

        for(int cnt = 0; cnt < _msl_host_count; cnt++) {
	  if((_msl[cnt] != NULL) &&
	     (strcmp(((PigMSL *)_msl[cnt])->getOrigHostID(), host_id+1) == 0))
	      // we got a match
	      return _msl[cnt];	    
	}
  	
	// No match has been found, so create new MSL mission object
	_msl[_msl_host_count] = new PigMSL(mission, host_id+1);
	
	if(_msl[_msl_host_count] == NULL) {
	    printStaticMsg("Error allocating MSL Mission object!", 
			   PigMsgFatal);
	    return &_defaultObject;
	}

	//increment the count
	_msl_host_count++;

        return _msl[_msl_host_count-1];

    }

    if(mission != NULL && (strncasecmp(mission, "NSYT", 3) == 0)) {
	const char *host_id;

	// For NSYT mission name is of the form: "mission:host_id"
	// separate them into two separate strings.
	host_id = strchr(mission, ':');

        for(int cnt = 0; cnt < _nsyt_host_count; cnt++) {
	  if((_nsyt[cnt] != NULL) &&
	     (strcmp(((PigNSYT *)_nsyt[cnt])->getOrigHostID(), host_id+1) == 0))
	      // we got a match
	      return _nsyt[cnt];	    
	}
  	
	// No match has been found, so create new NSYT mission object
	_nsyt[_nsyt_host_count] = new PigNSYT(mission, host_id+1);
	
	if(_nsyt[_nsyt_host_count] == NULL) {
	    printStaticMsg("Error allocating NSYT Mission object!", 
			   PigMsgFatal);
	    return &_defaultObject;
	}

	//increment the count
	_nsyt_host_count++;

        return _nsyt[_nsyt_host_count-1];

    }

    if(mission != NULL && (strncasecmp(mission, "PHX", 3) == 0)) {
	const char *host_id;

	// For PHX mission name is of the form: "mission:host_id"
	// separate them into two separate strings.
	host_id = strchr(mission, ':');

        for(int cnt = 0; cnt < _phx_host_count; cnt++) {
	  if((_phx[cnt] != NULL) &&
	     (strcmp(((PigPHX *)_phx[cnt])->getHostID(), host_id+1) == 0))
	      // we got a match
	      return _phx[cnt];	    
	}
  	
	// No match has been found, so create new PHX mission object
	_phx[_phx_host_count] = new PigPHX(mission, host_id+1);
	
	if(_phx[_phx_host_count] == NULL) {
	    printStaticMsg("Error allocating PHX Mission object!", 
			   PigMsgFatal);
	    return &_defaultObject;
	}

	//increment the count
	_phx_host_count++;

        return _phx[_phx_host_count-1];

    }
  
    if(mission != NULL && (strncasecmp(mission, "MER", 3) == 0)) {

	const char *host_id;

	// For MER mission name is of the form: "mission:host_id"
	// separate them into two separate strings.
	host_id = strchr(mission, ':');

        for(int cnt = 0; cnt < _mer_host_count; cnt++) {
	  if((_mer[cnt] != NULL) &&
	     (strcmp(((PigMER *)_mer[cnt])->getHostID(), host_id+1) == 0))
	      // we got a match
	      return _mer[cnt];	    
	}
  	
	// No match has been found, so create new MER mission object
	_mer[_mer_host_count] = new PigMER(mission, host_id+1);
	
	if(_mer[_mer_host_count] == NULL) {
	    printStaticMsg("Error allocating MER Mission object!", 
			   PigMsgFatal);
	    return &_defaultObject;
	}

	//increment the count
	_mer_host_count++;

        return _mer[_mer_host_count-1];
    }
  
    if(mission != NULL && (strcasecmp(mission, "FIDO") == 0)) {
        if(_fido == NULL) {
	    _fido = new PigFIDO();
	    if(_fido == NULL) {
	        printStaticMsg("Error allocating Fido Mission object!", 
			       PigMsgFatal);
		return &_defaultObject;
	    }
	}
	return _fido;
    }

    if (mission != NULL && (strcasecmp(mission, "M01") == 0)) {
        if (_m01 == NULL) {
	    _m01 = new PigM01();
	    if (_m01 == NULL) {		// whoops!
		printStaticMsg("Error allocating M01 Mission object!",
			       PigMsgFatal);
		return &_defaultObject;
	    }
	}
	return _m01;
    }

// Mars 98 removed 3/17/07 rgd to get around link problems with PHX.

#if 0
    if (mission != NULL && (strcasecmp(mission, "M98") == 0)) {
        if (_m98 == NULL) {
	    _m98 = new PigM98();
	    if (_m98 == NULL) {		// whoops!
		printStaticMsg("Error allocating M98 Mission object!",
			       PigMsgFatal);
		return &_defaultObject;
	    }
	}
	return _m98;
    }
#endif

    if (mission != NULL && (strcasecmp(mission, "MPF") == 0)) {
	if (_mpf == NULL) {
	    _mpf = new PigMPF();
	    if (_mpf == NULL) {		// whoops!
		printStaticMsg("Error allocating MPF Mission object!",
								PigMsgFatal);
		return &_defaultObject;
	    }
	}
	return _mpf;
    }

    if (mission != NULL && (strncasecmp(mission, "COLDARM", 7) == 0)) {
	const char *host_id;

	// For Coldarm mission name is of the form: "mission:host_id"
	// separate them into two separate strings.
	host_id = strchr(mission, ':');

        for (int cnt = 0; cnt < _coldarm_host_count; cnt++) {
	  if ((_coldarm[cnt] != NULL) &&
	     (strcmp(((PigColdarm *)_coldarm[cnt])->getHostID(), host_id+1) == 0))
	      // we got a match
	      return _coldarm[cnt];	    
	}
  	
	// No match has been found, so create new Coldarm mission object
	_coldarm[_coldarm_host_count] = new PigColdarm(mission, host_id+1);
	
	if (_coldarm[_coldarm_host_count] == NULL) {
	    printStaticMsg("Error allocating Coldarm Mission object!", 
			   PigMsgFatal);
	    return &_defaultObject;
	}

	//increment the count
	_coldarm_host_count++;

        return _coldarm[_coldarm_host_count-1];

    }

    if (mission != NULL && (strncasecmp(mission, "PSYCHE", 6) == 0)) {
	const char *host_id;

	// For Psyche mission name is of the form: "mission:host_id"
	// separate them into two separate strings.
	host_id = strchr(mission, ':');

        for (int cnt = 0; cnt < _psyche_host_count; cnt++) {
	  if ((_psyche[cnt] != NULL) &&
	     (strcmp(((PigPsyche *)_psyche[cnt])->getHostID(), host_id+1) == 0))
	      // we got a match
	      return _psyche[cnt];	    
	}
  	
	// No match has been found, so create new Coldarm mission object
	_psyche[_psyche_host_count] = new PigPsyche(mission, host_id+1);
	
	if (_psyche[_psyche_host_count] == NULL) {
	    printStaticMsg("Error allocating Psyche Mission object!", 
			   PigMsgFatal);
	    return &_defaultObject;
	}

	//increment the count
	_psyche_host_count++;

        return _psyche[_psyche_host_count-1];

    }

#if 0
    char msg[256];
    sprintf(msg, "Unknown mission type: '%s'", mission ? mission : "NULL");
    printStaticMsg(msg, PigMsgError);
    return &_defaultObject;
#endif

    // If it's not a known mission, it must be a generic image

    if (_generic_image == NULL) {
	_generic_image = new PigGenericImage();
	if (_generic_image == NULL) {		// whoops!
	    printStaticMsg("Error allocating Generic Image Mission object!",
								PigMsgFatal);
	    return &_defaultObject;
	}
    }
    return _generic_image;
}

////////////////////////////////////////////////////////////////////////
// This default is sufficient, if no mission-specific subclass is needed
////////////////////////////////////////////////////////////////////////

PigLabelModel *PigMission::createLabelModel(int unit)
{
  return new PigLabelModel(unit, getMissionName());
}

////////////////////////////////////////////////////////////////////////
// This default is sufficient, if no mission-specific subclass is needed
////////////////////////////////////////////////////////////////////////

PigFileModel *PigMission::createFileModel(const char *filename, int unit)
{
    return new PigFileModel(filename, unit, getMissionName());
}


////////////////////////////////////////////////////////////////////////
// Create pointing model given an image file.
////////////////////////////////////////////////////////////////////////

PigPointingModel *PigMission::createPointingModel(PigCameraModel *cm, PigFileModel *file,
                                                  const char *type, bool allow_type_override)
{
    char point_method[256];
    char type_lbl[50];
    char *value = NULL;
    int count;

    const char *inst_id = file->getInstrumentId();

    getParam("POINT_METHOD", point_method, &count, 1, 0);

    // check if command flag is saying don't do this
    if (count != 0)
        value = parseParamString(point_method, "NO_LBL_POINT");

    // if NO_LBL_POINT is not set
    if (value == NULL) {
        const char *pointingModelName = file->getPointingModelName();
        // Pointing Model is defined in the label and there is no 
        // command line restriction on using it
        if ( pointingModelName != NULL )
            return createPointingModel(cm, inst_id, pointingModelName, TRUE);
    }

    return createPointingModel(cm, inst_id, type, allow_type_override);

}

////////////////////////////////////////////////////////////////////////
// Create a surface model given an image file.
////////////////////////////////////////////////////////////////////////

PigSurfaceModel *PigMission::createSurfaceModel(PigFileModel *file)
{
    char coord[20];
    int count;
    PigCoordSystem *cs;
    PigModelBase::getStaticParam("SURF_COORD", coord, &count, 1, 0);
    if (count == 1)  //param present on a command line
        cs = getCoordSystem(file, coord);
    else // no param given on command line
        cs = getDefaultSurfaceCS(file, file->getInstrumentName());
	
    return createSurfaceModel(file->getInstrumentName(), NULL, cs);
}

////////////////////////////////////////////////////////////////////////
// Create default coordinate system for surface model.  Could be overridden
// by subclasses.
////////////////////////////////////////////////////////////////////////
PigCoordSystem *PigMission::getDefaultSurfaceCS(PigFileModel *file,
						const char *instrument)
{
    if (file == NULL)
        return getFixedCS();
    return getCoordSystem(file, "FIXED");
}

////////////////////////////////////////////////////////////////////////
// Create a surface model given strings.  
///////////////////////////////////////////////////////////////////////
PigSurfaceModel *PigMission::createSurfaceModel(const char *instrument,
                                            const char *target)
{
    return createSurfaceModel(instrument, target, NULL);
}
////////////////////////////////////////////////////////////////////////
//
////////////////////////////////////////////////////////////////////////
PigSurfaceModel *PigMission::createSurfaceModel(const char *instrument,
                                                const char *target,
                                                PigCoordSystem *cs)
{
    return createSurfaceModel(instrument, target, NULL, NULL, NULL, NULL, cs);
}


////////////////////////////////////////////////////////////////////////
// Create a surface model given strings.  
//
// This routine requires the following parameters be in the PDF:
//
// PARM SURFACE TYPE=KEYWORD COUNT=(0:1) +
// VALID=("INFINITY","PLANE","SPHERE1", "SPHERE2","MESH") DEFAULT=-- 
// PARM NORMAL TYPE=REAL COUNT=(0:3) DEFAULT=--
// PARM GROUND TYPE=REAL COUNT=(0:3) DEFAULT=--
// PARM SURF_COORD TYPE=STRING COUNT=(0:1) DEFAULT=--
// PARM SURF_MESH TYPE=STRING COUNT=(0:1) DEFAULT=--
// PARM SURF_CSFILE TYPE=STRING COUNT=(0:1) DEFAULT=--
//
// SURFACE specifies whether to use infinity, or a tilted plane, a sphere or
// a mesh as the surface model.  For PLANE only, NORMAL indicates how the plane
// is tilted (the quaternion is already taken into account).  The default is 
// (0,0,-1) since +Z is down.  Also for PLANE only, GROUND defines a ground 
// point through which the plane passes.  Any point on the plane may be chosen,
// although it will usually be directly underneath the coordinate origin. For
// SPHERE, the center of the sphere is defined from GROUND and the radius is
// defined in the X (first) component of the NORMAL vector. For MESH, PLANE and
// NORMAL are not used. The name of the OBJ file is retrieved from SURF_MESH 
// parameter.
//
// Order of priorities:
// 1. Command line arguments
//    If at least one surface model command line argument is provided,
//    then ignore ALL surface defining input parameters(type, normal,
//    ground, cs).  Use Default Values(via getDefault*** methods) to
//    to complete definition of surface model.  For example if
//    GROUND and NORMAL were specified on a command line, then the surface
//    model will be created with that ground and normal values and type,and cs
//    will be initialized using getDefaultSurfaceType() and getDefaultSurfaceCS
//       
// 2. If no command line args present, use input param values.  Complete surface
//    model definition with defaults as in #1
//
////////////////////////////////////////////////////////////////////////
PigSurfaceModel *PigMission::createSurfaceModel(const char *instrument,
						const char *target,
                                                const char *type,
                                                PigVector *normal,
                                                PigPoint *ground, 
                                                const char *mesh,
						PigCoordSystem *cs)
{
    int count;
    char surface[20];
    char coord[20];
    double vector[3];
    bool command_line_parms_present = false;
    
    // use only these variables to create surface object
    char surface_mesh[PIG_MAX_FILENAME_SIZE]; 
    char surface_mesh_raw[PIG_MAX_FILENAME_SIZE]; 
    char surface_coord_file[PIG_MAX_FILENAME_SIZE]; 
    char surface_coord_file_raw[PIG_MAX_FILENAME_SIZE]; 
    char surface_type[20];
    char surface_coord[20];
    PigVector *surface_normal;
    PigPoint  *surface_ground;
    PigCoordSystem *surface_cs;


    // Check normal
    PigModelBase::getStaticParam("NORMAL", vector, &count, 3, 0);
    if (count == 3) {
      command_line_parms_present = true;
      surface_normal = new PigVector(vector);
    }
    else
      surface_normal = getDefaultSurfaceNormal(instrument);


    // Check ground
    PigModelBase::getStaticParam("GROUND", vector, &count, 3, 0);
    if (count == 3) {
        command_line_parms_present = true;
        surface_ground = new PigPoint(vector);
    }
    else
        surface_ground = getDefaultSurfaceGround(instrument);


    //check surface type
    PigModelBase::getStaticParam("SURFACE", surface_type, &count, 1, 0);
    if (count == 1) {
        command_line_parms_present = true;
    }
    else {
      getDefaultSurfaceType(instrument, surface_type);
      }
    

    // Check mesh
    PigModelBase::getStaticParam("SURF_MESH", surface_mesh_raw, &count, 1, 0);
    if (count == 1) {
        command_line_parms_present = true;
        zvfilename(surface_mesh_raw, surface_mesh, sizeof(surface_mesh)-1);
    }
    else
        strcpy(surface_mesh, "none\0");


    //check surface cs
    PigModelBase::getStaticParam("SURF_COORD", surface_coord, &count, 1, 0);
    //!!!! here we make an assumption that if command line parameter was provided,
    //cs was created in this.createSurfaceModel(file) and passed in this method,
    // so we just use it.  The reason that we have to do this is that definition
    // of surface cs on a command line is incomplete i.e. we can't construct proper
    // cs object by just knowing it's name.
    // This is a temporary hack that treats SURF_COORD parameter differently from
    // other surface model creation arguments. -ozp 
    if (count == 1 && cs) {
	command_line_parms_present = true;
        surface_cs = cs;        
    }
    else
     	surface_cs = getDefaultSurfaceCS(NULL, instrument);


    // Ultimately, the user can supply a file, whose CS in the label is 
    // extracted and applied to the surface. This parameter for surface CS file
    // was introduced for the user to be able to provide a CS corresponding to 
    // a mesh file, but doesn't necessarily requires a mesh for the surface
    // definition 
    // This CS supersede all other CS (default (COORD) or SURF_COORD)
    PigModelBase::getStaticParam("SURF_CSFILE", surface_coord_file_raw, &count, 1, 0);
    if (count == 1 ) {
       command_line_parms_present = true;
       zvfilename(surface_coord_file_raw, surface_coord_file, sizeof(surface_coord_file)-1);
       PigFileModel * fm = PigFileModel::create(surface_coord_file);
       if (fm == NULL) {
          printStaticMsg("Unable to open SURF_CSFILE:", PigMsgWarning);
          printStaticMsg(surface_coord_file, PigMsgWarning);
       }
       else {
          PigMission * m = getMissionObject(fm->getMissionName());
          PigRoverStateManager * rsm = m->getRoverStateManager();
          if (rsm)
             rsm->addFileCoordSystems(fm);
          PigCSReference * ref;
          fm->getDerivedImageCS(ref);
          surface_cs = m->getCoordSystem(ref);
          fm->closeFile();
          delete fm;
       }
    }


    if (!command_line_parms_present) {
      if (type)
	strcpy(surface_type, type);
      if (cs)
      	surface_cs = cs;
      if (normal)
        surface_normal = normal;
      if (ground)
        surface_ground = ground;	
      if (mesh)
         strcpy(surface_mesh, mesh);
    }


    if (strcasecmp(surface_type, "INFINITY") == 0)
        return (PigSurfaceModel *) new PigSurfaceInfinity(NULL, surface_cs);
    if (strcasecmp(surface_type, "SPHERE2") == 0) {
        // Radius of a sphere is stored in X component of "NORMAL"
        // "GROUND" point is the origin of a sphere
        double radius = surface_normal->getX();
        return (PigSurfaceModel *) new PigSurfaceSphere2(radius, 
							 *surface_ground, surface_cs);
    }
    if (strcasecmp(surface_type, "SPHERE1") == 0) {
        // Radius of a sphere is stored in X component of "NORMAL"
        // "GROUND" point is the origin of a sphere
        double radius = surface_normal->getX();
        return (PigSurfaceModel *) new PigSurfaceSphere1(radius, 
							 *surface_ground, surface_cs);
    }
    if (strcasecmp(surface_type, "MESH") == 0) {
// Mesh support activated depending on OS arch -  See comment at top of file
#if USES_PIG_MESH
        // Mesh .OBJ file name is stored in "SURF_MESH"
        return (PigSurfaceModel *) new PigSurfaceMesh(surface_cs, surface_mesh);
#else
        printStaticMsg("Mesh surface model requested but not available in this build. Defaulting to Plane", PigMsgWarning);
#endif
    }

    // default to plane...

    return (PigSurfaceModel *) new PigSurfacePlane(*surface_normal, *surface_ground, surface_cs);
}
//////////////////////////////////////////////////////////////////////////
// Creates surface model based on type, pointing parameteres and coordinate
// system.  Returns NULL if type is unknown.  Also Returns NULL if
// allow_override=true and surface model parameters have been specified 
// as command line arguments. The reason for this that specification of 
// of surface model on a command line might be incomplete and might require
// mission specific default values for surface type, ground or normal values.
// So the caller of this function should check return value and if it's
// NULL call other surface model creation method such as 
// createSurfaceModel(file)
//////////////////////////////////////////////////////////////////////////
PigSurfaceModel  *PigMission::createSurfaceModel(const char *instrument,
						 const char *type, 
						 const double params[], 
						 const int count,
						 PigCoordSystem *cs)
{   
   // check for command line parameters
   int cnt = 0;
   bool command_line_parm_present = false;        
   char surface[20];
   char surface_coord[20]; 
   
   //create surface model
   PigSurfaceModel *sm = createSurfaceModel(instrument, NULL, type, NULL, NULL, NULL, cs); 
      
   // Determine if surface model, at least partially has been specified on command line
   // If it was, don't apply pointing correction
   PigModelBase::getStaticParam("SURFACE", surface, &cnt, 1, 0);
   if (cnt > 0)
       command_line_parm_present = true;

    double vector[3];
    PigModelBase::getStaticParam("NORMAL", vector, &cnt, 3, 0);
    if (cnt == 3)
        command_line_parm_present = true;
    PigModelBase::getStaticParam("GROUND", vector, &cnt, 3, 0);
    if (cnt == 3)
        command_line_parm_present = true;      
        PigModelBase::getStaticParam("SURF_COORD", surface_coord, &cnt, 1, 0);
    if (cnt == 1)
        command_line_parm_present = true;
	 	 
     if (!command_line_parm_present)      
         // set Pointing parameters 
	 sm->setPointingParameters(params, count);
	    
     printStaticMsg("Created Surface Model with parameters listed below:", PigMsgInfo);
     sm->print();

     return sm;
}


////////////////////////////////////////////////////////////////////////
// Create a camera model given strings.  Valid strings are given in the
// camera mapping file (e.g. "NAV_LEFT").
// version: Serial Number of the camera, from the INSTRUMENT_SERIAL_NUMBER
//          label.  
// subtype: Filter number, focus, zoom, and temperature, in the form
//	    "filter=5 focus=1520 zoom=1022 temp=-25.345 partner_temp=-26.665".
//	    Filter comes from FILTER_NUMBER, or is "NULL" if not available.
//	    Focus is 0 if not available/applicable.
//	    Temp is this camera temperature, partner_temp is for the stereo
//	    partner (needed for proper linearization).
//	    Zoom is 0 if not available/applicable.
// special: standard string (across all subclasses).  "stereo" will return
//          the "stereo partner" of the model instead.
// construction:   How camera model is derived.
// calibration_id: The value of the CALIBRATION_SOURCE_ID in the Camera Model
//                 label.
// type: Type of camera model, like cahv, cahvor or cahvore  
//
// All strings are case-insensitive.
//
// If the PigCoordSystem is NULL, the "natural" frame for the given
// mission/instrument, at the default site, are used.
//
// Note:  This version will not interpolate unless the subtype string
// is set up with the interpolant values.  The File version of create
// should always be used in preference to this.  Valid uses of this are
// to create output models that approximate the inputs, NOT to model an
// input file.
////////////////////////////////////////////////////////////////////////

PigCameraModel *PigMission::createCameraModelInterp(const char *instrument,
                                          const char *version, 
                                          const char *subtype, 
                                          const char *special,
                                          const char *construction, 
                                          const char *calibration, 
                                          PigCoordSystem *cs)
{
    char msg[1024];
    int stereo;
    int status;
    PigCameraModel *model = NULL;
    PigCameraMapper *map = NULL;
    PigCameraMapEntry *entry = NULL;

    if (cs == NULL)                            // use default, natural frame
        cs = getCoordSystem(getNaturalFrame(instrument));
    else
        cs = getCoordSystem(cs, getNaturalFrame(instrument));
    if (cs == NULL)
        PigModelBase::printUniqueStaticMsg("Unable to find any CS!", PigMsgWarning);


    // Check the "special" string
    stereo = 0;
    if (special != NULL && strlen(special) > 0) {
        if (strcasecmp(special, "stereo") == 0)
            stereo = 1;
        else
            return NULL;                // unknown special string
    }

    // Find out the name of camera model file
    PigXerces::initialize();

    map = new PigCameraMapper(NULL, getHostID());
    
    if (!map)
        return NULL;  // can't create mapper for given hostid 

    if (stereo) {               // swap instrument names
        entry = map->findFromStereoPartnerID(instrument);
    }
    else {
        // Having Version is a rare case, usually it's null
        if (version) {
            entry = map->findFromSerialNumber(version);
            // check for consistency with instrument id
            if(!strcmp(entry->getName(), instrument)) {
                sprintf(msg, "Warning PigCameraMapEntry.getName=%s, does not match instrument_name= %s",
                        entry->getName(), instrument);
		PigModelBase::printStaticMsg(msg, PigMsgWarning);
            }
              
        }

        // This is the most common case: based on instrument name
        // we find the map entry.
        else if(instrument)
            entry = map->findFromID(instrument);
    } 

    if (entry) {
        // We create an instance now then fill in the parameters later.
        model = createCameraModel(entry, subtype, construction, calibration);
	
	if(!model)
	    return NULL;
    }
    else {
        // No Entry Found!
        sprintf(msg, 
                "No Entry Found in Camera Mapping XML file for %s Host_Id: %s, instrument: %s", 
                getMissionName(),getHostID(),instrument);
        PigModelBase::printStaticMsg(msg, PigMsgFatal);
        return NULL;
    }
    
    char *subtype_copy = NULL;
    
    if (cs != NULL)
        model->setInitialCoordSystem(cs);
    
    if ((entry->getFilters() || entry->getFocus() || entry->getTemperature() ||
	entry->getZoom()) && subtype) {
        subtype_copy = strdup(subtype);
    }
    
    status = getCameraModelParmsFromFile(model,
					 cs,
					 calibration,
					 entry->getSerialNumber(),
					 subtype_copy,
					 entry->getType(),
					 entry->getFocus(),
					 entry->getZoom(),
					 entry->getTemperature(), stereo);
 
    if (!status)
        return NULL;   // We failed to read camera model's parms data from file

    return adjustCameraModel(model, cs, map, entry, calibration,
			     construction, subtype_copy, stereo);
}

//////////////////////////////////////////////////////////////////////
// Construct the string of calibration camera model filename
// Here we allow any of the strings to be NULL.
// Returns status: SUCESS = 1, FAIL = 0
// interp_focus or interp_temp or interp_zoom should be TRUE if the camera
// uses interpolation.
// interp_partner_temp says to use the partner temp instead of main temp.
// Note that interp_temp must also be on or this is ignored.
//////////////////////////////////////////////////////////////////////
int PigMission::getCameraModelParmsFromFile(PigCameraModel *model,
					PigCoordSystem *cs,
					const char *calibration,
					const char *serial,
					const char *subtype,
					const char *type,
					int interp_focus,
					int interp_zoom,
					int interp_temp,
					int interp_partner_temp)
{
    int status = 1;
    char cmod_file[PIG_MAX_FILENAME_SIZE+1];
    int does_interp = interp_focus || interp_temp || interp_zoom;

    // First we attempt to find camera model cal. file
    // using "serial", "subtype" and "type"
    constructCameraModelFileName(calibration, serial, 
		subtype, type, interp_focus, interp_zoom,
		interp_temp, interp_partner_temp,
		true, cmod_file);
    status = readCameraModelFromFile(model, cs, subtype,
		interp_focus, interp_zoom, interp_temp, interp_partner_temp,
		cmod_file);
    if (status == 1)  // success reading the file
        return status;

    // BACK-UP: Try to find calibration camera model by discarding filter
    constructCameraModelFileName(calibration, serial,
		subtype, type, interp_focus, interp_zoom,
		interp_temp, interp_partner_temp,
		false, cmod_file);
    status = readCameraModelFromFile(model, cs, subtype,
		interp_focus, interp_zoom, interp_temp, interp_partner_temp,
		cmod_file);
    if (status == 1)
        return status;

    // 2nd BACK-UP: Now discard interpolation, but retain filter.
    constructCameraModelFileName(calibration, serial,
		subtype, type, false, false, false, false, true, cmod_file);
    status = readCameraModelFromFile(model, cs, NULL,
		false, false, false, false, cmod_file);
    if (status == 1)
        return status;

    // Final BACK-UP: Now discard both filter and interpolation.
    constructCameraModelFileName(calibration, serial,
		NULL, type, false, false, false, false, false, cmod_file);
    status = readCameraModelFromFile(model, cs, NULL,
		false, false, false, false, cmod_file);
    if (status == 1)
        return status;

    // if we are here, all our attempts to find calibration
    // camera model files have failed. 
    PigModelBase::printStaticMsg(
		"Unable to find calibration camera model file!", PigMsgFatal);
    return status; 

}

///////////////////////////////////////////////////////////////
// Given filename string(cmod_file) read camera model from that
// Returns status: SUCCESS = 1, FAIL = 0
//
// Interpolation is handled here.  If either interp is true, then
// the filename is not actually a camera model, but an interp file.
///////////////////////////////////////////////////////////////
int PigMission::readCameraModelFromFile(PigCameraModel *model,
				    PigCoordSystem *cs,
				    const char *subtype,
				    int interp_focus,
				    int interp_zoom,
				    int interp_temp,
				    int interp_partner_temp,
				    char *cmod_file)
{
    static int in_interp = FALSE;
    static int printed = FALSE;
    char cmod_path[1024];
    int status = 1;

    if (!model || !cs)  // check for NULL values
        return 0;

    if (interp_focus || interp_temp || interp_zoom) {
	in_interp = TRUE;
	int status = doInterpolation(model, cs, subtype, cmod_file,
		interp_focus, interp_zoom, interp_temp, interp_partner_temp);
	in_interp = FALSE;
	return status;
    }

    FILE *f = PigModelBase::openConfigFile(cmod_file, cmod_path);
    if (f != NULL) {
        fclose(f);
        status = model->readFromFile(cmod_path, cs);
    }
  
    char msg[1024];  
    if (f == NULL || status != 0) {             // file not present
        sprintf(msg,
                "Unable to find calibration camera model file: %s (path=%s)",
                cmod_file, cmod_path);
        PigModelBase::printUniqueStaticMsg(msg, PigMsgWarning);
	return 0;
    }
    else {
        sprintf(msg,
                "Successfully read calibration camera model from file: %s (path=%s)",
                cmod_file, cmod_path);
	if (!in_interp || !printed) {		// don't inundate with messages
            PigModelBase::printUniqueStaticMsg(msg, PigMsgInfo);
	    printed = TRUE;
	}
	
	model->setInterpType("NONE");

	return 1;
    }
}

//////////////////////////////////////////////////////////////////////
// Do the interpolation for a camera model.  Reads in the interp file,
// all the camera models it points to, and does the actual interpolation.
//
// The filename is not a camera model file but a .interp file.
// which looks like this:
//
// TYPE
// n
// f1 model_name_1
// f2 model_name_2
// ...
//
// where TYPE is the type of interpolation (FOCUS or ZOOM or TEMPERATURE),
// n is the number of models (lines in the file), f1 is an
// integer specifying the dependent variable for this model for
// interpolation, and model_name_1 is the actual model filename.
// If TYPE is not given, and the first line resolves to a non-0 integer,
// then FOCUS type is assumed.  This is for backwards compatibility.
//
// Returns 1 on success, 0 on failure.
//
// Note that it is possible to do more than one type of interpolation.
// The filename is ".interpf" for focus or ".interpt" for temperature
// or ".interpz" for zoom, or some combination (e.g. ".interpft", ".interpfzt").
// If a combination, what happens is that the file specifies via the first
// line which interpolation to do.  The model names in the file should then
// specify the *other* interpolation (e.g. if type is FOCUS then
// the names would be .interpz for zoom).  Those files are then
// recursively read by readCameraModelFromFile, which will call this again.
// So the second variable (in this case zoom) is interpolated first,
// and then the results are interpolated by the first (focus).
// An error results if the same interp type is given at both levels.
//////////////////////////////////////////////////////////////////////

int PigMission::doInterpolation(PigCameraModel *model,
				PigCoordSystem *cs,
				const char *subtype,
				char *interp_file,
				int interp_focus,
				int interp_zoom,
				int interp_temp,
				int interp_partner_temp)
{
    char interp_path[1024];
    char msg[1024];
    int n_models = 0;
    PigCameraModel *interp_models[PIG_MAX_INTERP_CAMERA_MODELS];
    double interp_value[PIG_MAX_INTERP_CAMERA_MODELS];
    int i, status;
    int is_focus = false;	// comes from the file itself
    int is_zoom = false;
    int is_temp = false;

    if (subtype == NULL)
	return 0;			// shouldn't happen

    // Extract the focus value to use

    char filt[100];		// don't care
    double focus = 0;		// read as float for maximum flexibility
    double zoom = 0;
    double temperature = 0;
    double partner_temp = 0;
    if (subtype) {
        sscanf(subtype,"filter=%s focus=%lf zoom=%lf temp=%lf partner_temp=%lf",
			filt, &focus, &zoom, &temperature, &partner_temp);
    }

    // Open the interp file

    FILE *f = PigModelBase::openConfigFile(interp_file, interp_path);
    if (f == NULL) {
	sprintf(msg, "Unable to find interp camera model file: %s (path=%s)",
			interp_file, interp_path);
	PigModelBase::printUniqueStaticMsg(msg, PigMsgWarning);
	return 0;
    }

    char line[2048];
    line[0] = '#';
    while (line[0] == '#') {		// skip comments
	char *l = fgets(line, sizeof(line), f);
	if (l == NULL) {
	    sprintf(msg, "Premature end of interp file: %s (path=%s)",
				interp_file, interp_path);
	    PigModelBase::printStaticMsg(msg, PigMsgWarning);
	    fclose(f);
	    return 0;
	}
    }

    // Whichever one we're doing, turn that off for the recursive call below.

    if (strncasecmp(line, "FOCUS", 5) == 0) {
	if (!interp_focus) {
	    sprintf(msg, "Interp file %s (path=%s) specifies FOCUS but FOCUS interpolation is not enabled",
			interp_file, interp_path);
	    PigModelBase::printStaticMsg(msg, PigMsgError);
	    fclose(f);
	    return 0;
	}
	is_focus = true;
	interp_focus = false;
    }
    if (strncasecmp(line, "ZOOM", 4) == 0) {
	if (!interp_zoom) {
	    sprintf(msg, "Interp file %s (path=%s) specifies ZOOM but ZOOM interpolation is not enabled",
			interp_file, interp_path);
	    PigModelBase::printStaticMsg(msg, PigMsgError);
	    fclose(f);
	    return 0;
	}
	is_zoom = true;
	interp_zoom = false;
    }
    if (strncasecmp(line, "TEMPERATURE", 11) == 0) {
	if (!interp_temp) {
	    sprintf(msg, "Interp file %s (path=%s) specifies TEMPERATURE but TEMPERATURE interpolation is not enabled",
			interp_file, interp_path);
	    PigModelBase::printStaticMsg(msg, PigMsgError);
	    fclose(f);
	    return 0;
	}
	is_temp = true;
	interp_temp = false;
    }
    if (is_focus || is_temp || is_zoom) {
        line[0] = '#';
        while (line[0] == '#') {		// skip comments
	    char *l = fgets(line, sizeof(line), f);
	    if (l == NULL) {
	        sprintf(msg, "Premature end of interp file: %s (path=%s)",
				    interp_file, interp_path);
	        PigModelBase::printStaticMsg(msg, PigMsgWarning);
	        fclose(f);
	        return 0;
	    }
	}
    }

    sscanf(line, "%d", &n_models);

    if (n_models <= 0 || n_models > PIG_MAX_INTERP_CAMERA_MODELS) {
	sprintf(msg, "Unreasonable interp n_models=%d from file %s",
				n_models, interp_path);
	PigModelBase::printStaticMsg(msg, PigMsgWarning);
	fclose(f);
	return 0;
    }

    for (i=0; i < n_models; i++) {
	interp_models[i] = model->clone();

	line[0] = '#';
	while (line[0] == '#') {		// skip comments
	    char *l = fgets(line, sizeof(line), f);
	    if (l == NULL) {
		sprintf(msg, "Premature end of interp file: %s", interp_path);
		PigModelBase::printStaticMsg(msg, PigMsgWarning);
		fclose(f);
		return 0;
	    }
	}
	char fn[1024];
	sscanf(line, "%lf %s", &interp_value[i], fn);

	// Read this specific model.  We already have the filename

	status = readCameraModelFromFile(interp_models[i], cs, subtype,
		interp_focus, interp_zoom, interp_temp, interp_partner_temp,fn);
	if (status == 0) {		// msg already issued
	    fclose(f);
	    return 0;
	}
    }

    fclose(f);		// done reading!

    // Interpolate the models.

    float interp_to = -1e30;
    char *interp_string = "Unknown";		// should not ever be seen
    if (is_focus) {
	interp_to = focus;
	interp_string = "Focus";
        status = model->interpolateModels(n_models, interp_models, interp_value,
			focus);
    }
    else if (is_zoom) {
	interp_to = zoom;
	interp_string = "Zoom";
	status = model->interpolateModels(n_models, interp_models, interp_value,
			zoom);
    }
    else if (is_temp) {
	interp_to = temperature;
	interp_string = "Temperature";
	if (interp_partner_temp)
	    interp_to = partner_temp;
        status = model->interpolateModels(n_models, interp_models, interp_value,
			temperature);
    }
    else {
	sprintf(msg,
		"Internal error: invalid interp settings in doInterpolation");
	PigModelBase::printStaticMsg(msg, PigMsgFatal);
	return 0;
    }

    if (status == 0) {
	sprintf(msg, "Failure interpolating camera model");
	PigModelBase::printStaticMsg(msg, PigMsgWarning);
	return 0;
    }

    sprintf(msg, "Successfully interpolated model for %s to %f from %s (path=%s)",
                interp_string, interp_to, interp_file, interp_path);
    PigModelBase::printUniqueStaticMsg(msg, PigMsgInfo);

    model->setInterpValue(interp_to);
    if (is_focus)
	model->setInterpType("FOCUS");
    else if (is_zoom)
	model->setInterpType("ZOOM");
    else
	model->setInterpType("TEMPERATURE");

    return 1;
}

////////////////////////////////////////////////////////////////////////
// Using the info in construction string, modify input camera model
// for "Linearized", "Subframed", "Downsampled" images.
// If this is already the stereo partner, set is_stereo to true.  That's
// in case the stereo partner needs to be linearized, which would cause
// us to find the original as the stereo partner of this.  Which is fine
// except we lose track of which interpolation to use.  is_stereo lets us
// use the right interpolant.
////////////////////////////////////////////////////////////////////////

PigCameraModel* PigMission::adjustCameraModel(PigCameraModel *camera_in,
					  PigCoordSystem *cs,
					  PigCameraMapper *map,
					  PigCameraMapEntry *entry,
					  const char *calibration,
					  const char *construction,
					  const char *subtype,
					  int is_stereo)
{
    // Adjust the model for subframing and/or downsampling 
    // and/or linearized type.
    int dx, dy, hscale, vscale;
    char cmod_file[PIG_MAX_FILENAME_SIZE+1];
    PigCameraMapEntry *partner_entry = NULL;
    PigCameraModel *partner_cm = NULL;
    PigCameraModel *camera_out = NULL;
    int status;

    // set defaults
    dx = dy = 0;
    hscale = vscale = 1;

    if (!camera_in)
        return NULL;

    // Read the construction string
    char proj_type[11];
    strcpy(proj_type, "");
    sscanf(construction,"kinematics type=%s subframe=(%d,%d) downsampling=(%d,%d)",
	   &proj_type, &dx, &dy, &hscale, &vscale);
    
    if (!strcmp(proj_type, "LINEARIZED")) {
        if (map)
	    partner_entry = map->findStereoPartner(entry);

	if (partner_entry) {

	    partner_cm = createCameraModel(partner_entry,
					   subtype,
					   construction,
					   calibration);
	    if (partner_cm) {
	        partner_cm->setInitialCoordSystem(cs);

		// If this is a normal image, is_stereo is false so we pass
		// in true to get the stereo partner.  If this is already the
		// stereo partner, is_stereo is true so we pass in false to
		// get the interpolant from the original image.
		status = getCameraModelParmsFromFile(partner_cm,
					cs,
					calibration,
					partner_entry->getSerialNumber(),
					subtype,
					partner_entry->getType(),
					partner_entry->getFocus(),
					partner_entry->getZoom(),
					partner_entry->getTemperature(),
					!is_stereo);
		if (status != 1) {
		    delete partner_cm;
		    partner_cm = NULL;
		}
	    }
	}

	camera_out = camera_in->alignStereoCameras(
					entry->getNS(), entry->getNL(),
					entry->getNS(), entry->getNL(),
					entry->getNS(), entry->getNL(),
					partner_cm);       
    }
    else {
      // for "RAW" output camera is the same as an input
      camera_out = camera_in;
	}
    
    // Note that both shiftCamera() and shiftScale() modify the
    // *current* set of CM parameters.	We must copy these to
    // the *initial* set so they can be the "calibration" model
    // for pointing.

    // shift the camera_out parameters for any subframing.
    // if both dx and dy are 0 we don't need a shift
    if ((dx > 0) || (dy > 0)) {
      camera_out->shiftCamera(dx, dy);
      camera_out->setInitialFromCurrent();
    }
    
    // scale the camera_out parameters for any downsampling
    // similarly to the above: don't need a scale if either
    // parameter is equal to 0 or both equal to 1
    if ((hscale != 0) && (vscale != 0) &&
	((hscale != 1) || (vscale != 1))) {
      camera_out->scaleCamera(1.0/hscale, 1.0/vscale);
      camera_out->setInitialFromCurrent();
    }

    return camera_out;
}        

////////////////////////////////////////////////////////////////////////
// Get the Fixed CS.  Sets it if not set already.
////////////////////////////////////////////////////////////////////////

PigCoordSystem *PigMission::getFixedCS()
{
    if (getFixedCSInternal() != NULL)
	return getFixedCSInternal();

    // Nobody is claiming to be Fixed, so set it to something....

    setFixedCS();
    return getFixedCSInternal();
}

////////////////////////////////////////////////////////////////////////
// Sets the fixed CS.  Uses FIXED_SITE parameter if given, or the
// lowest-numbered site in both the internal CS database and the 
// solution list if not (where "site" is defined as anything with 
// only one RMC index, unless FIXED_NAME is given in which case that
// has to match).  If all else fails, look for anything with 
// 0 indices, and then anything at all.
//
// Looks for the following in the PDF (although will work fine without:
// PARM FIXED_SITE TYPE=INTEGER COUNT=0:1 DEFAULT=--
// PARM FIXED_NAME TYPE=STRING COUNT=0:1 DEFAULT=--
////////////////////////////////////////////////////////////////////////

void PigMission::setFixedCS()
{
    int min = 100000;
    int min_index = -1;

    PigRoverStateManager *rsm = getRoverStateManager();

    // See if FIXED_SITE given

    int fixed_request = -1;
    int count;
    getParam("FIXED_SITE", &fixed_request, &count, 1, 0);
    if (count == 0)
	fixed_request = -1;		// not requested

    // See if FIXED_NAME given

    char fixed_name[256];
    int fixed_name_count = 0;
    getParam("FIXED_NAME", fixed_name, &fixed_name_count, 1, 0);

    // If the user asked for a specific fixed site, use it

    if (fixed_request != -1) {

	if (fixed_name_count == 0) {
	    //!!!! There should be a better way to get this default...
	    strcpy(fixed_name, "SITE_FRAME");
	}

	PigCSReference srref(this, fixed_name, NULL, &fixed_request,1,NULL);

        PigCSDefinition *scsdef = rsm->findSolution(&srref);
	if (scsdef != NULL) {
	    PigCoordSystem *cs = getCoordSystem(&srref);
	    if (cs != NULL) {
		setFixedCS(cs);
		return;
	    }
	}

	// Create new CS with that ref site, no reference, identity values
	PigCSDefinition srdef(this,&srref,NULL,PigVector(),PigQuaternion());
        rsm->addCoordSys(&srdef);
	PigCoordSystem *cs = getCoordSystem(&srref);
	if (cs != NULL) {
	    setFixedCS(cs);
	    return;
	}
	printError("Unable to create specific FIXED request, trying other options");
    }

    PigCoordSystem **list = getCSList();
    int n = *getCSCounter();
    for (int i=0; i < n; i++) {
	if (list[i]->getNumIndices() == 1) {
	    // If we asked for a name and this ain't it, continue
	    if (fixed_name_count != 0 && 
		    strcasecmp(list[i]->getFrameName(), fixed_name) != 0) {
		continue;
	    }
	    if (fixed_request != -1 && fixed_request == list[i]->getIndex(0)) {
		min = list[i]->getIndex(0);	// not really min but...
		min_index = i;
		break;				// stop looking
	    }
	    if (list[i]->getIndex(0) < min) {
		min = list[i]->getIndex(0);
		min_index = i;
	    }
	}
    }

    // See if we get a better min from the solution system

    int min2 = min;
    if (min_index == -1)		// not found
	min2 = -1;

    PigCSDefinition *sol = rsm->findPotentialFixedCS(min2, fixed_request);

    if (sol != NULL) {		// means it found a better one
	PigCoordSystem *fixed_cs = getCoordSystem(sol->getIdentity());
        setFixedCS(fixed_cs);
	return;
    }

    if (min_index != -1) {
	
        if (fixed_request != -1 && min != fixed_request) {
	    char msg[1024];
	    sprintf(msg, "Requested Fixed site %d not found, using %d instead",
		fixed_request, min);
	    printError(msg);
        }
        setFixedCS(list[min_index]);
	return;
    }

    // None found, look for no indices (this is not an error condition yet)

    for (int i=0; i < n; i++) {
	if (list[i]->getNumIndices() == 0) {
	    min_index = i;
	    break;
	}
    }
    if (min_index != -1) {
        setFixedCS(list[min_index]);
	return;
    }

    // Still none found... look for a site that's a reference (but obviously
    // is not an actual CS)

    list = getCSList();
    n = *getCSCounter();
    min = 100000;
    min_index = -1;
    for (int i=0; i < n; i++) {
	PigCSReference *rref = list[i]->getReference();
	if (rref == NULL)
	    continue;
	if (rref->getNumIndices() == 1) {
	    if (rref->getIndex(0) < min) {
		min = list[i]->getIndex(0);
		min_index = i;
	    }
	}
    }

    if (min_index != -1) {
	// Create new CS with that ref site, no reference, identity values
	PigCSReference *rref = list[min_index]->getReference();
	PigCSDefinition rdef(this, rref, NULL, PigVector(), PigQuaternion());
        rsm->addCoordSys(&rdef);
	PigCoordSystem *cs = getCoordSystem(rref);
	if (cs != NULL) {
	    setFixedCS(cs);
	    return;
	}
    }

    // Still nothing... find the lowest index0 in the DB

    PigCSReference fixr(this, "FIXED", NULL, NULL, 0, NULL);
    PigCSDefinition *fixd = rsm->findSolution(&fixr);

    // This is a bit of a hack... create a CSRef using SITE and the given
    // index from the fixed search.  Then create a CS def from it as above.

    if (fixd != NULL) {

        // Really found a 0-index fixed!

	if (fixd->getIdentity()->getNumIndices() == 0) {
	    PigCoordSystem *cs = getCoordSystem(&fixr);
	    if (cs != NULL) {
		setFixedCS(cs);
		return;
	    }
	} else {
	    // implement the hack

            int fix_rmc[1] = { fixd->getIdentity()->getIndex(0) };
	    PigCSReference fix_hack(this, "SITE", NULL, fix_rmc, 1, NULL);
	    PigCSDefinition fd(this, &fix_hack, NULL, PigVector(),
						PigQuaternion());
            rsm->addCoordSys(&fd);
	    PigCoordSystem *cs = getCoordSystem(&fix_hack);
	    if (cs != NULL) {
	        setFixedCS(cs);
	        return;
	    }
	}
    }

    // Last-ditch effort... look for a reference in the solution DB

    PigCSReference *last_ditch = rsm->findSolutionRef(&fixr);
    if (last_ditch != NULL) {
	// similar hack to the above: create a CS entry for it
	PigCSDefinition ditch(this, last_ditch, NULL, PigVector(),
						PigQuaternion());
        rsm->addCoordSys(&ditch);
	PigCoordSystem *cs = getCoordSystem(last_ditch);
	if (cs != NULL) {
	    setFixedCS(cs);
	    return;
	}
    }


    // Okay I give up... find anything (this IS an error)

    printError("Unable to find appropriate Fixed CS, using first available");
    min_index = 0;
    if (*getCSCounter() == 0) {
	printError("No CS's available!");
	return;
    }
    setFixedCS(list[min_index]);
}

////////////////////////////////////////////////////////////////////////
// Sets the given CS to be the Fixed CS
////////////////////////////////////////////////////////////////////////

void PigMission::setFixedCS(PigCoordSystem *cs)
{
    setFixedCSInternal(cs);

    invalidateAllCSCaches();
}

////////////////////////////////////////////////////////////////////////
// Sets the given CSRef to be the Fixed CS.  If the CS pointed at by
// this CSRef does not exist, it is created with an identity transform
// and no reference frame.  Since we ARE the Fixed CS, no ref is needed.
// The use case here is the telemprocs, which create the Rover CS pointing
// to Site and then expect Site frame to exist (as the fixed) too.
//
// We duplicate some of getCoordSystem() here just to avoid error messages
// if not found.
////////////////////////////////////////////////////////////////////////

void PigMission::setFixedCS(PigCSReference *ident)
{
    if (ident == NULL) {
	setFixedCS();		// try anything...
	return;
    }

    // Special case: FIXED

    if (strcmp(ident->getFrameName(), "FIXED_FRAME") == 0) {
	getFixedCS();
	return;
    }

    PigCoordSystem **csl = getCSList();
    int num_cs = *getCSCounter();

    for (int i=0; i < num_cs; i++) {
	if (ident->isEqual(csl[i]->getIdentity())) {
	    setFixedCS(csl[i]);
	    return;
	}
    }

    PigCoordSystem *cs;

    // Not found, so look in RSM 

    PigRoverStateManager *rsm = getRoverStateManager();

    PigCSDefinition *csdef = rsm->findSolution(ident);
    if (csdef != NULL) {	// let that routine create...
	cs = getCoordSystem(ident);
	setFixedCS(cs);
	return;
    }

    // Still not found, so create an "empty" one

    csdef = new PigCSDefinition(this, ident, NULL, PigVector(),PigQuaternion());
    getRoverStateManager()->addCoordSys(csdef);
    delete csdef;
    cs = getCoordSystem(ident);		// "should" work now...
    if (cs == NULL) {		// whoops!
	setFixedCS();		// try anything...
	return;
    }
    setFixedCS(cs);
}

////////////////////////////////////////////////////////////////////////
// Invalidate all CS caches
////////////////////////////////////////////////////////////////////////

void PigMission::invalidateAllCSCaches()
{
    PigCoordSystem **list = getCSList();
    int n = *getCSCounter();
    for (int i=0; i < n; i++) {
	list[i]->invalidateCache();
    }
}

////////////////////////////////////////////////////////////////////////
// Returns or creates if needed the coordinate system matching the given
// reference.  First looks for a CS that already exists, then looks in the
// RoverStateManager for one that's eligible for creation.
//
// This should be the only place in the system a CS object is actually created.
////////////////////////////////////////////////////////////////////////
PigCoordSystem *PigMission::getCoordSystem(PigCSReference *ident)
{
    if (ident == NULL)
        return NULL;

    // Special case: FIXED

    if (strcmp(ident->getFrameName(), "FIXED_FRAME") == 0) {
	return getFixedCS();
    }

    PigCoordSystem **csl = getCSList();
    int num_cs = *getCSCounter();

    for (int i=0; i < num_cs; i++) {
	if (ident->isEqual(csl[i]->getIdentity()))
	    return csl[i];

    }

    // Not found, so look in RSM 

    PigRoverStateManager *rsm = getRoverStateManager();

    PigCSDefinition *csdef = rsm->findSolution(ident);
    if (csdef == NULL) {

	// One last try before erroring.  Set up the fixed CS.  This might
	// create e.g. a Site object that is Fixed, which we could then find.

	if (getFixedCSInternal() == NULL) {
	    PigCoordSystem *fixed = getFixedCS();
	    if (fixed != NULL && ident->isEqual(fixed->getIdentity())) {
		return fixed;
	    }
	}

	char msg[1024];
	sprintf(msg, "Unable to create coord system for %s",
					ident->getFullName());
	printError(msg);
	return NULL;
    }

    // Object adds itself to the cslist during creation

    PigCoordSystem *cs = new PigCoordSystem(this, csdef);

    return cs;
}

////////////////////////////////////////////////////////////////////////
// Create a CS using RMC info from the given file. NULL returns the
// "natural" frame for the given file (typically a rover frame or
// equivalent) and is equivalent to specifying "instrument".
////////////////////////////////////////////////////////////////////////

PigCoordSystem *PigMission::getCoordSystem(PigFileModel *file,
					const char *frame)
{
    if (file == NULL)			// huh??!
	return getCoordSystem(frame);

    const char *f = frame;
    if (f == NULL)
        f = getNaturalFrame(file->getInstrumentId());

    // get the RMC

    int rmc[PIG_MAX_RMC_INDEX], rmc_count;

    rmc_count = PIG_MAX_RMC_INDEX;
    file->getRoverMotionCounter(rmc, rmc_count);

    // If this is actually a mosaic (of XYZ's, say), then there might not
    // be an RMC.  Get it from the CS definition in the file with the most
    // number of indices (because it's the most specific).

    if (rmc_count == 0) {
	PigCSDefinition *cs_defs[MAX_CS_OBJ];
	int num_cs_defs = MAX_CS_OBJ;
	PigFileModel::readAllCoordSystems(this, file->getUnit(),
					num_cs_defs, cs_defs);
	for (int i=0; i < num_cs_defs; i++) {
	    PigCSReference *cs_ref = cs_defs[i]->getIdentity();
	    if (cs_ref->getNumIndices() > rmc_count) {
		rmc_count = cs_ref->getNumIndices();
		for (int j=0; j < rmc_count; j++) {
		   rmc[j] = cs_ref->getIndex(j);
		}
	    }
	}
    }

#if 0		//!!!! NOSITE has been disabled
    // Check for NOSITE

    char nosite[16];
    int count;
    getParam("NOSITE", nosite, &count, 1, sizeof(nosite));
    if (count != 0) {           // NOSITE specified, blank out RMC info
	for (int k=0; k<rmc_count; k++)
	    rmc[k] = 0;
    }
#endif

    // Create the CSREF for the desired CS

    PigCSReference *csref = new PigCSReference(this, f, NULL,
				rmc, rmc_count, file->getInstrumentId());

    PigCoordSystem *cs = getCoordSystem(csref);

    delete csref;

    return cs;
}


////////////////////////////////////////////////////////////////////////
// Returns whatever match it finds for the given CS name in the
// list of currently-instantiated CS objects.  If not found, it searches
// the PigRoverStateManager to find any csdef with the given frame
// name.
// THIS ONE SHOULD BE AVOIDED.  It tacitly assumes that there is only
// one RMC value in the CS databases, which is often not the case.  If
// there is more than one, it is arbitrary which is returned.  Also,
// NULL and INSTRUMENT are not allowed for the frame name on this one.
////////////////////////////////////////////////////////////////////////

PigCoordSystem *PigMission::getCoordSystem(const char *frame)
{
    // Create a CSREF to canonicalize the name, even though there's no indices

    PigCSReference *csref = new PigCSReference(this, frame, NULL, NULL, 0,NULL);

    PigCoordSystem **csl = getCSList();
    int num_cs = *getCSCounter();

    for (int i=0; i < num_cs; i++) {
	if (strcmp(csref->getFrameName(), csl[i]->getFrameName()) == 0) {
	    return csl[i];			// found a match
	}
    }

    // No match, let it look for a matching name (with num indices == 0 it'll
    // just find whatever it can).

    PigCoordSystem *cs = getCoordSystem(csref);
    delete csref;
    return cs;
}

////////////////////////////////////////////////////////////////////////
// Returns a CS using the same index set as the given CS but a 
// different frame name
////////////////////////////////////////////////////////////////////////

PigCoordSystem *PigMission::getCoordSystem(PigCoordSystem *templ,
							const char *frame)
{
    int index[PIG_MAX_CS_INDEX];
    int num = templ->getNumIndices();
    for (int i=0; i < num; i++)
	index[i] = templ->getIndex(i);

    //!!!! should we transfer over the solution ID?  Seems like no is
    //!!!! the better answer, unless we come across a use case...

    PigCSReference *csref = new PigCSReference(this, frame, NULL,
		index, num, templ->getIdentity()->getInstId());

    PigCoordSystem *cs = getCoordSystem(csref);
    delete csref;
    return cs;
}

///////////////////////////////////////////////////////////////////////
// Add constant coordinate systems related to this one to the RSM
// database.  Missions that need this should override.  Default
// implementation loads LOCAL_LEVEL when given ROVER or ROVER_NAV.
///////////////////////////////////////////////////////////////////////

void PigMission::addConstantCoordSystems(PigRoverStateManager *rsm,
                                         PigCSDefinition *csdef)
{
    PigCSReference *ident = csdef->getIdentity();
    const char *parent = ident->getFrameName();

    // Even though these are the canonical names, they still vary a lot
    // per mission...

    if ((strcasecmp(parent, "ROVER_NAV_FRAME") == 0) ||
	(strcasecmp(parent, "ROVER_FRAME") == 0) ||
	(strcasecmp(parent, "ROVER") == 0) ||
	(strcasecmp(parent, "LANDER_FRAME") == 0) ||
	(strcasecmp(parent, "PAYLOAD_FRAME") == 0) ||
	(strcasecmp(parent, "M01 Camera") == 0)) {

        // Add LOCAL_LEVEL.  Same offset as ROVER but no quat

        PigCSReference *ll_ref = new PigCSReference(
                this,
                "LOCAL_LEVEL_FRAME",
                ident->getSolutionId(),
                ident->getIndices(),
                ident->getNumIndices(),
                ident->getInstId());

        PigVector ll_offset = csdef->getOffset();

        PigQuaternion ll_quat;          // identity

        PigCSDefinition *ll = new PigCSDefinition(this, ll_ref,
                        csdef->getReference(), ll_offset, ll_quat);

        // addSolution will only add it if it's unique

        rsm->addSolution(ll);
        delete ll_ref;
        delete ll;
    }
}

