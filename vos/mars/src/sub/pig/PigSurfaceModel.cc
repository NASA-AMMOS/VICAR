////////////////////////////////////////////////////////////////////////
// PigSurfaceModel
//
// Base class for Surface models.  This models a surface, and intersects
// rays with the surface.  Examples of surfaces include a sphere or ellipsoid
// for orbiter images, a flat plane for landers, or an actual terrain map.
// The factory functions return the "standard" surface model for the given
// mission/camera/target or image.  Often, specific subclasses will be
// instantiated for the desired terrain.
//
// TBD:  are surfaces time-dependent for rotating modies (i.e. relationship
// to J2000)?
////////////////////////////////////////////////////////////////////////

#include "PigSurfaceModel.h"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigCoordSystem.h"
#include "PigCSReference.h"

#include "PigSurfaceInfinity.h"
#include "PigSurfacePlane.h"
#include "PigSurfaceSphere.h"
#include "PigSurfaceSphere1.h"
#include "PigSurfaceSphere2.h"
// Mesh support activated depending on OS arch - 
// Pig supports .obj mesh for surface model. The ray tracer used internally to
// intersect the mesh with viewing pixels is handled by the Intel Embree lib, 
// which is only available on 64-bit OSes. The mesh support is activated 
// accordingly. See imake_util.tmp and imake.config
#if USES_PIG_MESH
#include "PigSurfaceMesh.h"
#endif

#include "return_status.h"

#include <string.h>

////////////////////////////////////////////////////////////////////////
// Constructors
////////////////////////////////////////////////////////////////////////

PigSurfaceModel::PigSurfaceModel(char *mission, char *instrument, char *target)
{
    _mission = _instrument = _target = NULL;

    if (mission)
	_mission = strdup(mission);
    if (instrument)
	_instrument = strdup(instrument);
    if (target)
	_target = strdup(target);
    _cs = PigMission::getMissionObject(mission)->getFixedCS();
}

PigSurfaceModel::PigSurfaceModel(char *target, PigCoordSystem *cs)
{
    _mission = _instrument = _target = NULL;

    if (target)
	_target = strdup(target);
    _cs = cs;
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigSurfaceModel::~PigSurfaceModel()
{
    if (_mission)
	delete _mission;
    if (_instrument)
	delete _instrument;
    if (_target)
	delete _target;
}

////////////////////////////////////////////////////////////////////////
// These factory methods create and return an instance of the "standard"
// surface subclass for the given mission/instrument/target.  These
// parameters are specified by the file itself (look at the label to figure
// it out), or by the mission name/instrument/target name.  Applications
// should feel free to instantiate their own specific subclasses for
// specific desired surfaces rather than using these factories (as opposed
// to the camera and pointing models, where the factories are almost
// always used).
////////////////////////////////////////////////////////////////////////

PigSurfaceModel *PigSurfaceModel::create(const char *filename)
{
    PigFileModel *file = PigFileModel::create(filename);
    if (file == NULL)
	return NULL;
    PigSurfaceModel *surface = create(file);
    delete file;
    return surface;
}

PigSurfaceModel *PigSurfaceModel::create(PigFileModel *file)
{
    PigMission *m = PigMission::getMissionObject(file);
    return m->createSurfaceModel(file);
}

PigSurfaceModel *PigSurfaceModel::create(const char *mission,
					 const char *instrument, 
					 const char *target)
{
    PigMission *m = PigMission::getMissionObject(mission);
    return m->createSurfaceModel(instrument, target);
}
PigSurfaceModel *PigSurfaceModel::create(const char *mission,
					 const char *instrument,
					 const char *type, 
					 const double params[], 
					 const int count,
					 PigCoordSystem *cs)
{
    PigMission *m = PigMission::getMissionObject(mission);
    return m->createSurfaceModel(instrument, type, params, count, cs);

}
////////////////////////////////////////////////////////////////////////
// This is basically the inverse of intersectRay().  Don't know if it's
// useful.  Returns the look direction unit vector given the origin and
// an XYZ point on the surface.  The base class implements this via
// simple subtraction and normalization of the points, but e.g. a celestial
// sphere might behave differently.  If infinity_flag is true, the given
// point *is* the look vector (as returned by intersectRay(), and this is
// just returned.
// TBD:  is this needed????!!!!
////////////////////////////////////////////////////////////////////////

void PigSurfaceModel::getRay(const PigPoint &origin,
		const PigPoint &intersect_point,
		const int infinity_flag,
		PigVector &look_direction)
{
    if (infinity_flag)
	look_direction = intersect_point;
    else
	look_direction = intersect_point - origin;
    look_direction.normalize();
    return;
}

////////////////////////////////////////////////////////////////////////
// The label routines expect a VICAR unit number, already open in the proper
// mode (read/write).
////////////////////////////////////////////////////////////////////////

int PigSurfaceModel::readFromLabel(int unit, int instance)
{
    LblSurfaceModel_typ SurfaceModel;
    int status;

    status = LblSurfaceModelParms(unit, LBL_READ, &SurfaceModel, instance);
    if (RTN_FAILURE(status)) {
	printMsg((char *)LblErrorMessage(), "SurfaceModel", PigMsgError);
	return 1;
    }

    if (!SurfaceModel.SurfaceModelType.Valid) {
	printError("Missing SurfaceModelType in label");
	return 1;
    }
    if (strcasecmp(SurfaceModel.SurfaceModelType.Value,
				surfaceModelLabelType()) != 0) {
	char msg[150];
	sprintf(msg,
		"SurfaceModelType in label does not match subclass type of %s",
		surfaceModelLabelType());
	printError(msg);
	return 1;
    }

    useLabelStruct(&SurfaceModel);

    return 0;
}

////////////////////////////////////////////////////////////////////////
// The label routines expect a VICAR unit number, already open in the
// proper mode (read/write).  0==success, 1==failure on return.
// For writeToLabel, the caller may fill in fields in the aux structure
// if desired, which are not otherwise set by the label writer.  Set the
// Valid flag for any fields set.  Defaults are provided for required
// fields.  aux may be passed in as NULL.

int PigSurfaceModel::writeToLabel(int unit, int instance,
			const LblSurfaceModel_typ *aux) const
{
    LblSurfaceModel_typ SurfaceModel;
    int status;

    if (aux)
	memcpy(&SurfaceModel, aux, sizeof(LblSurfaceModel_typ));
    else
	memset(&SurfaceModel, 0, sizeof(LblSurfaceModel_typ));

    strcpy(SurfaceModel.SurfaceModelType.Value, surfaceModelLabelType());
    SurfaceModel.SurfaceModelType.Valid = 1;

    fillLabelStruct(&SurfaceModel);

    status = LblSurfaceModelParms(unit, LBL_WRITE, &SurfaceModel, instance);
    if (RTN_FAILURE(status)) {
	printMsg((char *)LblErrorMessage(), "SurfaceModel", PigMsgError);
	return 1;
    }

    return 0;
}

////////////////////////////////////////////////////////////////////////
// This factory method creates and returns a surface model from a label.
// It differs from readFromLabel because that is subclassed and expects
// a specific type of SM.  This factory looks to see what kind of SM to
// create first, then creates the appropriate type, and calls readFromLabel
// to fill it in.
//
// Returns NULL if the model could not be read for some reason.
////////////////////////////////////////////////////////////////////////

PigSurfaceModel *PigSurfaceModel::createFromLabel(PigFileModel *file)
{
    const LblSurfaceModel_typ *surfaceModel;
    PigSurfaceModel *sm;

    surfaceModel = file->getLblSurfaceModel();

    if (surfaceModel == NULL) {
	printStaticMsg("Unable to find surface model label", PigMsgError);
	return NULL;
    }

    if (!surfaceModel->SurfaceModelType.Valid) {
	printStaticMsg("Missing SurfaceModelType in surface model label",
							PigMsgError);
	return NULL;
    }
    if (strcasecmp(surfaceModel->SurfaceModelType.Value, "INFINITY") == 0)
	sm = new PigSurfaceInfinity((char *)file->getMissionName(),
		(char *)file->getInstrumentName(), (char *)NULL);

    else if (strcasecmp(surfaceModel->SurfaceModelType.Value, "PLANE") == 0)
	sm = new PigSurfacePlane((char *)file->getMissionName(),
		(char *)file->getInstrumentName(), (char *)NULL);

    else if (strcasecmp(surfaceModel->SurfaceModelType.Value, "SPHERE") == 0)
	sm = new PigSurfaceSphere((char *)file->getMissionName(),
 		(char *)file->getInstrumentName(), (char *)NULL);

    else if (strcasecmp(surfaceModel->SurfaceModelType.Value, "SPHERE1") == 0)
	sm = new PigSurfaceSphere1((char *)file->getMissionName(),
 		(char *)file->getInstrumentName(), (char *)NULL);

    else if (strcasecmp(surfaceModel->SurfaceModelType.Value, "SPHERE2") == 0)
	sm = new PigSurfaceSphere2((char *)file->getMissionName(),
 		(char *)file->getInstrumentName(), (char *)NULL);

    else if (strcasecmp(surfaceModel->SurfaceModelType.Value, "MESH") == 0) {
// Mesh support activated depending on OS arch -  See comment at top of file
#if USES_PIG_MESH
	sm = new PigSurfaceMesh((char *)file->getMissionName(),
 		(char *)file->getInstrumentName(), (char *)NULL);
#else
	printStaticMsg("Mesh surface model not available in this build", PigMsgError);
	return NULL;
#endif
    }

    else {
	char msg[256];
	sprintf(msg, "Unrecognized surface model type: '%s'",
				surfaceModel->SurfaceModelType.Value);
	printStaticMsg(msg, PigMsgError);
	return NULL;
    }

    // Get coord system from surface model label

    PigCSReference *ref;
    file->getSurfaceModelCS(ref);
    PigMission *m = PigMission::getMissionObject(sm->getMissionName());
    PigCoordSystem *cs = m->getCoordSystem(ref);
    sm->setCoordSystem(cs);

    // Now read the values from the label

    int status = sm->readFromLabel(file->getUnit(), 1);
    if (status != 0)		// error
	return NULL;

    return sm;
}

