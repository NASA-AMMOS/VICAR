////////////////////////////////////////////////////////////////////////
// PigPSPH
//
// Implements the PSPH camera model.
////////////////////////////////////////////////////////////////////////

#include "PigPSPH.h"
#include "PigCoordSystem.h"
#include "PigQuaternion.h"
#include "PigCSReference.h"
#include "PigFileModel.h"
#include "PigMission.h"
#include "return_status.h"
#include "cahvor.h"

#include <iostream>
using namespace std;
#include <string.h>
#include <math.h>

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigPSPH::PigPSPH(const char *mission, const char *instrument,
		 const char *version, const char *subtype,
		 const char *construction, const char *calibration)
	: PigCameraModel(mission, instrument, version, 
			 subtype, construction, calibration)
{
    _psph_set = FALSE;
    _dims_set = FALSE;
}

PigPSPH::PigPSPH()
	: PigCameraModel()
{
    _psph_set = FALSE;
    _dims_set = FALSE;
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigPSPH::~PigPSPH()
{
    // nothing to do...
}

////////////////////////////////////////////////////////////////////////
// Create a cmod_psph_t structure for use in calling the cmod routines.
////////////////////////////////////////////////////////////////////////

void PigPSPH::fill_psph_t(cmod_psph_t *psph) const
{
    _current_c.getXYZ(psph->c);
    _current_ax.getXYZ(psph->ax);
    _current_ay.getXYZ(psph->ay);
    _current_nx.getXYZ(psph->nx);
    _current_ny.getXYZ(psph->ny);
    psph->sx = _current_sx;
    psph->sy = _current_sy;
}

////////////////////////////////////////////////////////////////////////
// Move info from a cmod_psph_t struct back into the class
////////////////////////////////////////////////////////////////////////

void PigPSPH::empty_psph_t(cmod_psph_t *psph)
{
    _current_c.setXYZ(psph->c);
    _current_ax.setXYZ(psph->ax);
    _current_ay.setXYZ(psph->ay);
    _current_nx.setXYZ(psph->nx);
    _current_ny.setXYZ(psph->ny);
    _current_sx = psph->sx;
    _current_sy = psph->sy;
}

////////////////////////////////////////////////////////////////////////
// I/O routines
////////////////////////////////////////////////////////////////////////

int PigPSPH::readFromFile(const char *filename, PigCoordSystem *cs)
{
    cmod_int_t xdim;
    cmod_int_t ydim;
    cmod_psph_t psph;

    int status = cmod_psph_read(filename, &xdim, &ydim, &psph);

    if (status != CAHVOR_SUCCESS)
	return status;		// SUCCESS is 0 for CAHV

    setInitialPSPH(psph.c, psph.ax, psph.ay, psph.nx, psph.ny,
		   psph.sx, psph.sy, cs);		// CS not changed

    _xdim = xdim;
    _ydim = ydim;
    _dims_set = TRUE;

    return 0;
}

int PigPSPH::writeToFile(const char *filename) const
{
    cmod_psph_t psph;
    int xdim, ydim;
    char *comment = "Written from PigPSPH";

    fill_psph_t(&psph);

    if (_dims_set) {
	xdim = _xdim;
	ydim = _ydim;
    } else {
        xdim = 1024;			//!!!! unknown by this class
        ydim = 1024;
    }

    int status = cmod_psph_write(filename, comment, xdim, ydim, &psph);

    if (status != CAHVOR_SUCCESS)
	return status;		// SUCCESS is 0 for CAHV

    return 0;
}

////////////////////////////////////////////////////////////////////////
// The label routines expect a PigFileModel now.

int PigPSPH::readFromLabel(PigFileModel *file, int instance)
{
    const LblCameraModel_typ *CameraModel;
    PigCoordSystem *cs;
    PigCSReference *cs_ref;
    int status;

    CameraModel = file->getLblCameraModel(instance);
    if (CameraModel == NULL) {
        printError("Unable to read PSPH camera model from label!");
	return 1;
    }

    if (!CameraModel->ModelType.Valid) {
	printError("Missing ModelType in label");
	return 1;
    }
    if (strcasecmp(CameraModel->ModelType.Value, "PSPH") != 0) {
	printError("ModelType in label does not match subclass type of PSPH");
	return 1;
    }

    // Get coord sys reference from label

    status = file->getCameraModelCS(cs_ref, instance);
    // Ignore status.  We want to use the reference regardless.

    cs = PigMission::getMissionObject(file)->getCoordSystem(cs_ref);
    if (cs == NULL) {
	char msg[256];
	printWarning("Unable to find coord system reference.  Using default");
	sprintf(msg, "Bad reference is: %s", cs_ref->getFullName());
	printWarning(msg);

	cs = _initial_cs;			// just in case...
    }

    if (! (CameraModel->ModelComponent1[0].Valid &&         //C
	   CameraModel->ModelComponent1[1].Valid && 
	   CameraModel->ModelComponent1[2].Valid && 

	   CameraModel->ModelComponent2[0].Valid &&         //AX
	   CameraModel->ModelComponent2[1].Valid &&
	   CameraModel->ModelComponent2[2].Valid &&


	   CameraModel->ModelComponent3[0].Valid &&         //AY
	   CameraModel->ModelComponent3[1].Valid &&
	   CameraModel->ModelComponent3[2].Valid &&

	   CameraModel->ModelComponent4[0].Valid &&         //NX
	   CameraModel->ModelComponent4[1].Valid &&
	   CameraModel->ModelComponent4[2].Valid &&

	   CameraModel->ModelComponent5[0].Valid &&         //NY
	   CameraModel->ModelComponent5[1].Valid &&
	   CameraModel->ModelComponent5[2].Valid &&

	   CameraModel->ModelComponent6[0].Valid &&         //SX
	   CameraModel->ModelComponent7[0].Valid )) {	    //SY
	printError("ModelComponent(s) not valid in label");
	return 1;
    }

    // Silly label API uses floats, not doubles!!  So we have to treat each
    // component individually

    PigVector c(CameraModel->ModelComponent1[0].Value,
		CameraModel->ModelComponent1[1].Value,
		CameraModel->ModelComponent1[2].Value);
    PigVector ax(CameraModel->ModelComponent2[0].Value,
		 CameraModel->ModelComponent2[1].Value,
		 CameraModel->ModelComponent2[2].Value);
    PigVector ay(CameraModel->ModelComponent3[0].Value,
		 CameraModel->ModelComponent3[1].Value,
		 CameraModel->ModelComponent3[2].Value);
    PigVector nx(CameraModel->ModelComponent4[0].Value,
		 CameraModel->ModelComponent4[1].Value,
		 CameraModel->ModelComponent4[2].Value);
    PigVector ny(CameraModel->ModelComponent5[0].Value,
		 CameraModel->ModelComponent5[1].Value,
		 CameraModel->ModelComponent5[2].Value);

    double sx = CameraModel->ModelComponent6[0].Value;
    double sy = CameraModel->ModelComponent7[0].Value;

    setInitialPSPH(c,ax,ay,nx,ny,sx,sy, cs);

    _xdim = file->getNS();
    _ydim = file->getNL();
    _dims_set = TRUE;

    // Read ancillary info, if present

    _transformVectorValid = FALSE;
    _transformQuatValid = FALSE;
    if (CameraModel->ModelTransformVector.Valid) {
	_transformVector = PigVector(CameraModel->ModelTransformVector.Value);
	_transformVectorValid = TRUE;
    }
    if (CameraModel->ModelTransformQuaternion.Valid) {
	_transformQuat = PigQuaternion(CameraModel->ModelTransformQuaternion.Value);
	_transformQuatValid = TRUE;
    }

    _interpMethodValid = FALSE;
    _interpValueValid = FALSE;
    if (CameraModel->InterpolationMethod.Valid) {
	strcpy(_interpMethod, CameraModel->InterpolationMethod.Value);
	_interpMethodValid = TRUE;
    }
    if (CameraModel->InterpolationValue.Valid) {
	_interpValue = CameraModel->InterpolationValue.Value;
	_interpValueValid = TRUE;
    }

    return 0;
}

////////////////////////////////////////////////////////////////////////
// The label routines expect a VICAR unit number, already open in the
// proper mode (read/write).  0==success, 1==failure on return.
// For writeToLabel, the caller may fill in certain fields in the aux
// structure if desired:  CalibrationSourceId, ModelDesc,
// ModelName, GeometrySourceId.  Set the Valid flag for any fields
// set.  Defaults are provided for required fields.  aux may be passed
// in as NULL.

int PigPSPH::writeToLabel(int unit, int instance, PigCoordSystem *cs,
			  const LblCameraModel_typ *aux) const
{
    LblCameraModel_typ CameraModel;
    int status;

    if (aux)
	memcpy(&CameraModel, aux, sizeof(LblCameraModel_typ));
    else
	memset(&CameraModel, 0, sizeof(LblCameraModel_typ));

    setupWriteToLabel(CameraModel, cs);

    status = LblGeometricCameraModel(unit, LBL_WRITE, &CameraModel, instance);
    if (RTN_FAILURE(status)) {
        printMsg((char *)LblErrorMessage(), "CameraModel", PigMsgError);
	return 1;
    }
    return 0;
}

// Internal routine, used by subclasses to fill in basic PSPH info

void PigPSPH::setupWriteToLabel(LblCameraModel_typ &CM,PigCoordSystem *cs) const
{
    // Fill in required fields if user didn't supply

    if (!CM.CalibrationSourceId.Valid) {
	strcpy(CM.CalibrationSourceId.Value, "SYNTHETIC");
	CM.CalibrationSourceId.Valid = 1;
    }
    if (!CM.ReferenceCoordSystemName.Valid) {
	strcpy(CM.ReferenceCoordSystemName.Value, cs->getFrameName());
	CM.ReferenceCoordSystemName.Valid = 1;
    }

    PigCSReference *ident = cs->getIdentity();
    if (ident != NULL) {
	for (int cnt = 0; cnt < ident->getNumIndices(); cnt++) {
	    CM.ReferenceCoordSystemIndex[cnt].Value = ident->getIndex(cnt);
	    CM.ReferenceCoordSystemIndex[cnt].Valid = 1;
	}
	// Write solution_id string only if it's not NULL or "telemetry" 
	if (ident->getSolutionId() != NULL && 
	    strcasecmp(ident->getSolutionId(), "telemetry")) {
	    strcpy(CM.ReferenceCoordSystemSolnId.Value, ident->getSolutionId());
	    CM.ReferenceCoordSystemSolnId.Valid = 1;
	}
    }

    // Fill in constant fields for this model

    strcpy(CM.ModelType.Value, "PSPH");
    CM.ModelType.Valid = 1;

    // Fill in the fields for each component

    strcpy(CM.ModelComponentId[0].Value, "C");
    CM.ModelComponentId[0].Valid = 1;
    strcpy(CM.ModelComponentName[0].Value, "CENTER");
    CM.ModelComponentName[0].Valid = 1;
    strcpy(CM.ModelComponentUnit[0].Value, "METER");
    CM.ModelComponentUnit[0].Valid = 1;
    PigPoint c = cs->convertPoint(_current_c, _current_cs);

    CM.ModelComponent1[0].Value = c.getX();
    CM.ModelComponent1[1].Value = c.getY();
    CM.ModelComponent1[2].Value = c.getZ();
    CM.ModelComponent1[0].Valid = 1;
    CM.ModelComponent1[1].Valid = 1;
    CM.ModelComponent1[2].Valid = 1;

    strcpy(CM.ModelComponentId[1].Value, "AX");
    CM.ModelComponentId[1].Valid = 1;
    strcpy(CM.ModelComponentName[1].Value, "AXIS_X");
    CM.ModelComponentName[1].Valid = 1;
    strcpy(CM.ModelComponentUnit[1].Value, "N/A");
    CM.ModelComponentUnit[1].Valid = 1;
    PigVector ax = cs->convertVector(_current_ax, _current_cs);
    CM.ModelComponent2[0].Value = ax.getX();
    CM.ModelComponent2[1].Value = ax.getY();
    CM.ModelComponent2[2].Value = ax.getZ();
    CM.ModelComponent2[0].Valid = 1;
    CM.ModelComponent2[1].Valid = 1;
    CM.ModelComponent2[2].Valid = 1;

    strcpy(CM.ModelComponentId[2].Value, "AY");
    CM.ModelComponentId[2].Valid = 1;
    strcpy(CM.ModelComponentName[2].Value, "AXIS_Y");
    CM.ModelComponentName[2].Valid = 1;
    strcpy(CM.ModelComponentUnit[2].Value, "N/A");
    CM.ModelComponentUnit[2].Valid = 1;
    PigVector ay = cs->convertVector(_current_ay, _current_cs);
    CM.ModelComponent3[0].Value = ay.getX();
    CM.ModelComponent3[1].Value = ay.getY();
    CM.ModelComponent3[2].Value = ay.getZ();
    CM.ModelComponent3[0].Valid = 1;
    CM.ModelComponent3[1].Valid = 1;
    CM.ModelComponent3[2].Valid = 1;

    strcpy(CM.ModelComponentId[3].Value, "NX");
    CM.ModelComponentId[3].Valid = 1;
    strcpy(CM.ModelComponentName[3].Value, "NORMAL_X");
    CM.ModelComponentName[3].Valid = 1;
    strcpy(CM.ModelComponentUnit[3].Value, "N/A");
    CM.ModelComponentUnit[3].Valid = 1;
    PigVector nx = cs->convertVector(_current_nx, _current_cs);
    CM.ModelComponent4[0].Value = nx.getX();
    CM.ModelComponent4[1].Value = nx.getY();
    CM.ModelComponent4[2].Value = nx.getZ();
    CM.ModelComponent4[0].Valid = 1;
    CM.ModelComponent4[1].Valid = 1;
    CM.ModelComponent4[2].Valid = 1;

    strcpy(CM.ModelComponentId[4].Value, "NY");
    CM.ModelComponentId[4].Valid = 1;
    strcpy(CM.ModelComponentName[4].Value, "NORMAL_Y");
    CM.ModelComponentName[4].Valid = 1;
    strcpy(CM.ModelComponentUnit[4].Value, "N/A");
    CM.ModelComponentUnit[4].Valid = 1;
    PigVector ny = cs->convertVector(_current_ny, _current_cs);
    CM.ModelComponent5[0].Value = ny.getX();
    CM.ModelComponent5[1].Value = ny.getY();
    CM.ModelComponent5[2].Value = ny.getZ();
    CM.ModelComponent5[0].Valid = 1;
    CM.ModelComponent5[1].Valid = 1;
    CM.ModelComponent5[2].Valid = 1;

    strcpy(CM.ModelComponentId[5].Value, "SX");
    CM.ModelComponentId[5].Valid = 1;
    strcpy(CM.ModelComponentName[5].Value, "SCALE_X");
    CM.ModelComponentName[5].Valid = 1;
    strcpy(CM.ModelComponentUnit[5].Value, "radian/pixel");
    CM.ModelComponentUnit[5].Valid = 1;
    CM.ModelComponent6[0].Value = _current_sx;
    CM.ModelComponent6[0].Valid = 1;

    strcpy(CM.ModelComponentId[6].Value, "SY");
    CM.ModelComponentId[6].Valid = 1;
    strcpy(CM.ModelComponentName[6].Value, "SCALE_Y");
    CM.ModelComponentName[6].Valid = 1;
    strcpy(CM.ModelComponentUnit[6].Value, "radian/pixel");
    CM.ModelComponentUnit[6].Valid = 1;
    CM.ModelComponent7[0].Value = _current_sy;
    CM.ModelComponent7[0].Valid = 1;

    // Write ancillary info, if present

    if (_transformVectorValid) {
	double v[3];
	_transformVector.getXYZ(v);
	CM.ModelTransformVector.Value[0] = v[0];
	CM.ModelTransformVector.Value[1] = v[1];
	CM.ModelTransformVector.Value[2] = v[2];
	CM.ModelTransformVector.Valid = 1;
    }
    if (_transformQuatValid) {
	double q[4];
	_transformQuat.getComponents(q);
	CM.ModelTransformQuaternion.Value[0] = q[0];
	CM.ModelTransformQuaternion.Value[1] = q[1];
	CM.ModelTransformQuaternion.Value[2] = q[2];
	CM.ModelTransformQuaternion.Value[3] = q[3];
	CM.ModelTransformQuaternion.Valid = 1;
    }

    if (_interpMethodValid && strlen(_interpMethod) > 0) {
	strcpy(CM.InterpolationMethod.Value, _interpMethod);
	CM.InterpolationMethod.Valid = 1;
    }
    if (_interpValueValid) {
	CM.InterpolationValue.Value = _interpValue;
	CM.InterpolationValue.Valid = 1;
    }
}

////////////////////////////////////////////////////////////////////////
// Read the CM from a string.  For PSPH, only the PSPH vectors themselves
// are read.  Any error will return 1.  The parser is not very forgiving.

int PigPSPH::readFromString(const char *string, PigCoordSystem *cs)
{
    if (cs == NULL)
	cs = _initial_cs;

    double c[3], ax[3], ay[3], nx[3], ny[3], sx, sy;

    int status = sscanf(string,
	"PSPH: C=(%lf,%lf,%lf) AX=(%lf,%lf,%lf) AY=(%lf,%lf,%lf) NX=(%lf,%lf,%lf) NY=(%lf,%lf,%lf) SX=%lf SY=%lf",
		&c[0], &c[1], &c[2],
		&ax[0], &ax[1], &ax[2], &ay[0], &ay[2], &ay[2],
		&nx[0], &nx[1], &nx[2], &ny[0], &ny[1], &ny[2]);
    if (status != 17)		// 17 items to read
	return 1;

    setInitialPSPH(c, ax, ay, nx, ny, sx, sy, cs);

    return 0;
}

////////////////////////////////////////////////////////////////////////
// Write the CM to a string.  For PSPH, only the PSPH vectors themselves
// are written.

int PigPSPH::writeToString(char *string, int max_length)
{
    char buf[1023];

    sprintf(buf,
	"PSPH: C=(%lf,%lf,%lf) AX=(%lf,%lf,%lf) AY=(%lf,%lf,%lf) NX=(%lf,%lf,%lf) NY=(%lf,%lf,%lf) SX=%lf SY=%lf",
		_current_c.getX(), _current_c.getY(), _current_c.getZ(),
		_current_ax.getX(), _current_ax.getY(), _current_ax.getZ(),
		_current_ay.getX(), _current_ay.getY(), _current_ay.getZ(),
		_current_nx.getX(), _current_nx.getY(), _current_nx.getZ(),
		_current_ny.getX(), _current_ny.getY(), _current_ny.getZ(),
		_current_sx, _current_sy);
    if (strlen(buf) + 1 >= max_length)
	return 1;			// whoops, not big enough
    strcpy(string, buf);

    return 0;
}

////////////////////////////////////////////////////////////////////////
// Set the coordinate systems
////////////////////////////////////////////////////////////////////////

void PigPSPH::setInitialCoordSystem(PigCoordSystem *cs)
{
    if (_psph_set && cs != _initial_cs) {	// change initial params
	_orig_c = cs->convertPoint(_orig_c, _initial_cs);
	_orig_ax = cs->convertVector(_orig_ax, _initial_cs);
	_orig_ay = cs->convertVector(_orig_ay, _initial_cs);
	_orig_nx = cs->convertVector(_orig_nx, _initial_cs);
	_orig_ny = cs->convertVector(_orig_ny, _initial_cs);
	// sx, sy don't change
    }
    _initial_cs = cs;
    setCoordSystem(cs);
}

void PigPSPH::setCoordSystem(PigCoordSystem *cs)
{
    _current_c = cs->convertPoint(_current_c, _current_cs);
    _current_ax = cs->convertVector(_current_ax, _current_cs);
    _current_ay = cs->convertVector(_current_ay, _current_cs);
    _current_nx = cs->convertVector(_current_nx, _current_cs);
    _current_ny = cs->convertVector(_current_ny, _current_cs);
    // sx, sy don't change

    _current_cs = cs;
}

////////////////////////////////////////////////////////////////////////
// Set the initial PSPH vectors.  Note that the cs argument defines
// what the given vectors are measured in... it does NOT change
// the coordinate system definition of the object.  To do that, callers
// must set the CS first, then set the PSPH vectors.
////////////////////////////////////////////////////////////////////////

void PigPSPH::setInitialPSPH(const PigPoint c,
				const PigVector ax, const PigVector ay,
				const PigVector nx, const PigVector ny,
				double sx, double sy,
				PigCoordSystem *cs)
{
    _orig_c = _initial_cs->convertPoint(c, cs);
    _orig_ax = _initial_cs->convertVector(ax, cs);
    _orig_ay = _initial_cs->convertVector(ay, cs);
    _orig_nx = _initial_cs->convertVector(nx, cs);
    _orig_ny = _initial_cs->convertVector(ny, cs);
    _orig_sx = sx;
    _orig_sy = sy;
    _current_c = _current_cs->convertPoint(c, cs);
    _current_ax = _current_cs->convertVector(ax, cs);
    _current_ay = _current_cs->convertVector(ay, cs);
    _current_nx = _current_cs->convertVector(nx, cs);
    _current_ny = _current_cs->convertVector(ny, cs);
    _current_sx = sx;
    _current_sy = sy;

    _psph_set = TRUE;
}

void PigPSPH::setInitialPSPH(const double c[3],
				const double ax[3], const double ay[3],
				const double nx[3], const double ny[3],
				double sx, double sy,
				PigCoordSystem *cs)
{
    PigPoint cc(c);
    PigVector aax(ax);
    PigVector aay(ay);
    PigVector nnx(nx);
    PigVector nny(ny);
    setInitialPSPH(cc, aax, aay, nnx, nny, sx, sy, cs);
}

////////////////////////////////////////////////////////////////////////
// Set the current PSPH vectors.  Note that the cs argument defines
// what the given vectors are measured in... it does NOT change
// the coordinate system definition of the object.  To do that, callers
// must set the CS first, then set the PSPH vectors.
////////////////////////////////////////////////////////////////////////

void PigPSPH::setCurrentPSPH(const PigPoint c,
				const PigVector ax, const PigVector ay,
				const PigVector nx, const PigVector ny,
				double sx, double sy,
				PigCoordSystem *cs)
{
    _current_c = _current_cs->convertPoint(c, cs);
    _current_ax = _current_cs->convertVector(ax, cs);
    _current_ay = _current_cs->convertVector(ay, cs);
    _current_nx = _current_cs->convertVector(nx, cs);
    _current_ny = _current_cs->convertVector(ny, cs);
    _current_sx = sx;
    _current_sy = sy;
}

void PigPSPH::setCurrentPSPH(const double c[3],
				const double ax[3], const double ay[3],
				const double nx[3], const double ny[3],
				double sx, double sy,
				PigCoordSystem *cs)
{
    PigPoint cc(c);
    PigVector aax(ax);
    PigVector aay(ay);
    PigVector nnx(nx);
    PigVector nny(ny);

    setCurrentPSPH(cc, aax, aay, nnx, nny, sx, sy, cs);
}

////////////////////////////////////////////////////////////////////////
// Get the current PSPH vectors
////////////////////////////////////////////////////////////////////////

void PigPSPH::getCurrentPSPH(PigPoint &c, PigVector &ax, PigVector &ay,
			PigVector &nx, PigVector &ny, double &sx, double &sy)
{
    c = _current_c;
    ax = _current_ax;
    ay = _current_ay;
    nx = _current_nx;
    ny = _current_ny;
    sx = _current_sx;
    sy = _current_sy;
}

////////////////////////////////////////////////////////////////////////
// Relocate a camera model, based on initial and final positions and
// orientations of a reference point, which is rigidly connected to the
// camera but is otherwise arbitrary.  Usually this will be the axis
// of rotation for the actuators.  This should normally be called only
// by a PointingModel.  The PigCoordSystem indicates what the four given
// parameters are measured in; they are converted internally to match
// the current CS as necessary.
////////////////////////////////////////////////////////////////////////

void PigPSPH::moveCamera(const PigPoint initial_point,
			 const PigQuaternion initial_orientation,
			 const PigPoint final_point,
			 const PigQuaternion final_orientation,
			 PigCoordSystem *cs)
{
    double p_i[3], q_i[4];
    double p_f[3], q_f[4];
    cmod_psph_t psph_i;
    cmod_psph_t psph_f;

    if (!_psph_set) {
	printError("Attempt to modify uninitialized PSPH camera model");
	return;
    }

    if (cs != _current_cs) {
	_current_cs->convertPoint(initial_point, cs).getXYZ(p_i);
	_current_cs->convertQuat(initial_orientation, cs).getComponents(q_i);
	_current_cs->convertPoint(final_point, cs).getXYZ(p_f);
	_current_cs->convertQuat(final_orientation, cs).getComponents(q_f);
    }
    else {
	initial_point.getXYZ(p_i);
	initial_orientation.getComponents(q_i);
	final_point.getXYZ(p_f);
	final_orientation.getComponents(q_f);
    }

    fill_psph_t(&psph_i);

    cmod_psph_move(p_i, q_i, &psph_i, p_f, q_f, &psph_f);

    empty_psph_t(&psph_f);
}

///////////////////////////////////////////////////////////////////////
// This function scales a camera model. The scale factors should be
// understood as the same scale factors that would be applied to a 2D
// coordinate in the original model to convert it to a coordinate in
// the resulting model.
// Note that we must shift by a half a pixel before and after the scale
// to compensate for the pixel address being in the center of the pixel.
// See email "Cmod scaling and shifting" from Todd Litwin, 2011/03/30.
///////////////////////////////////////////////////////////////////////

void PigPSPH::scaleCamera(const double x_factor, 
			  const double y_factor)
{
    cmod_psph_t psph_i;
    cmod_psph_t psph_f;

    fill_psph_t(&psph_i);

    // Swap models back and forth here to avoid copying...

    cmod_psph_shift(-0.5, -0.5, &psph_i, &psph_f);
    cmod_psph_scale(x_factor, y_factor, &psph_f, &psph_i);
    cmod_psph_shift(0.5, 0.5, &psph_i, &psph_f);

    empty_psph_t(&psph_f);

    // The # of lines/samples probably changed as a result of the scale,
    // so we'll just say they're unavailable now.

    _dims_set = FALSE;
}

///////////////////////////////////////////////////////////////////////
// This function shifts a camera model. The shift values should be
// understood as the coordinates of the original model where the origin
// will fall in the new one.  In another words first_line, first_sample
// is the start of the image w.r.t. the Full Frame.
///////////////////////////////////////////////////////////////////////

void PigPSPH::shiftCamera(const double dx, 
			  const double dy)
{
    cmod_psph_t psph_i;
    cmod_psph_t psph_f;

    fill_psph_t(&psph_i);

    cmod_psph_shift(dx, dy, &psph_i, &psph_f);

    empty_psph_t(&psph_f);

    // The # of lines/samples probably changed as a result of the shift,
    // so we'll just say they're unavailable now.

    _dims_set = FALSE;
}

////////////////////////////////////////////////////////////////////////
// Aligns the model with another for use with stereo imagery.
//
// This is a no-op for PSPH - we currently only make PSPH's as a result
// of an alignment of other cameras, and there is no math for aligning
// PSPH's in the cmod routines.  So, we just return  clone of the input
// model.
////////////////////////////////////////////////////////////////////////

PigCameraModel *PigPSPH::alignStereoCameras(int xdim_in, int ydim_in,
					int xdim_out, int ydim_out,
					PigCameraModel *other)
{
    printWarning("alignStereoCameras is not implemented for PSPH models.  Models unchanged.");

    PigCameraModel *outputModel;

    //Create and Initialize output model.
    outputModel = clone();
    transferMetadata(outputModel);

    return outputModel;
}

////////////////////////////////////////////////////////////////////////
// Resets the camera pointing to the "reference" location (usually
// where the calibration was taken), in preparation for a new pointing.
// This resets the coordinate system as well.
////////////////////////////////////////////////////////////////////////

void PigPSPH::resetCameraLocation()
{
    _current_c = _orig_c;
    _current_ax = _orig_ax;
    _current_ay = _orig_ay;
    _current_nx = _orig_nx;
    _current_ny = _orig_ny;
    _current_sx = _orig_sx;
    _current_sy = _orig_sy;
    _current_cs = _initial_cs;
}

////////////////////////////////////////////////////////////////////////
// Copy the Current parameters to the Initial ("reference") model
// that is used by resetCameraLocation().  This is often used to
// modify the initial model for downsample/subframe before pointing
// takes place.
////////////////////////////////////////////////////////////////////////

void PigPSPH::setInitialFromCurrent()
{
    _orig_c = _current_c;
    _orig_ax = _current_ax;
    _orig_ay = _current_ay;
    _orig_nx = _current_nx;
    _orig_ny = _current_ny;
    _orig_sx = _current_sx;
    _orig_sy = _current_sy;
    _initial_cs = _current_cs;
}

////////////////////////////////////////////////////////////////////////
// Retrieves the image-space coordinates (LS below) of the "pointing
// axis" of the camera.  We do this by adding the approximate pointing
// direction to C and projecting that into the image. 
////////////////////////////////////////////////////////////////////////

void PigPSPH::getCameraCenter(double &line, double &sample) const
{
    PigVector a = getCameraOrientation();	// approximate! pointing
    PigPoint xyz = _current_c + a;

    XYZtoLS(xyz, FALSE, &line, &sample, _current_cs);

    return;
}

////////////////////////////////////////////////////////////////////////
// Returns the angle subtended by a pixel, in either the line or sample
// directions (normally these will be the same).  The angle is in radians,
// so the units are radians/pixel.  If this is not constant across the
// camera, the angle for the central pixel (looking down the pointing
// vector) is returned.  If "which" is 0, the Line direction is returned;
// 1 returns the Sample direction.
//
//!!!! There is probably a better closed-form way of doing this.  However,
//!!!! it's simple enough to just project two vectors and take the dot
//!!!! product.
////////////////////////////////////////////////////////////////////////

double PigPSPH::getPixelAngle(const int which) const
{
    if (!_psph_set) {
	printError("Attempt to use uninitialized PSPH camera model");
	return 0.0;
    }

    double line, samp;

    getCameraCenter(line, samp);

    PigPoint p1, p2;
    PigVector dir1, dir2;

    LStoLookVector(line, samp, p1, dir1, _current_cs);

    if (which == 0) {			// Line direction
	LStoLookVector(line+1, samp, p2, dir2, _current_cs);
    } else {
	LStoLookVector(line, samp+1, p2, dir2, _current_cs);
    }

    double dot = dir1 % dir2;

    return acos(dot);
}

////////////////////////////////////////////////////////////////////////
// These functions are the whole purpose behind this stuff.  Translate
// a line/samp into an origin and look direction vector, and from a
// 3D point into a line/samp location.  Note that the surface is not
// involved here; use the look vector with a SurfaceModel, or use a
// GeometryModel to do it all at once.  Note that the caller must
// pass in vector objects to be filled; these routines don't allocate
// them.  For the XYZtoLS case, if infinity_flag is true, the xyz point
// should actually be a unit vector pointing in the infinity direction,
// as returned by PigSurfaceModel::intersectRay().
//
// Note:  The passed-in coordinate system defines the input and output
// CS for the vectors.  They are converted to/from the internal CS as
// needed.  For efficiency, this should match the camera model's CS.
////////////////////////////////////////////////////////////////////////

void PigPSPH::LStoLookVector(const double line, const double sample,
			PigPoint &origin, PigVector &look_direction,
			PigCoordSystem *cs) const
{
    double ls[2];
    double pos[3], uvec[3];
    cmod_psph_t psph;

    if (!_psph_set) {
	printError("Attempt to use uninitialized PSPH camera model");
	return;
    }

    ls[0] = sample;
    ls[1] = line;

    fill_psph_t(&psph);

    cmod_psph_2d_to_3d(ls, &psph, pos, uvec, NULL);

    origin.setXYZ(pos);
    look_direction.setXYZ(uvec);

    if (cs != _current_cs && cs != NULL) {
	origin = cs->convertPoint(origin, _current_cs);
	look_direction = cs->convertVector(look_direction, _current_cs);
    }
}


void PigPSPH::XYZtoLS(const PigPoint &xyz_in, const int infinity_flag,
                      double *line, double *sample, PigCoordSystem *cs) const
{
   double range;
   XYZtoLS(xyz_in, infinity_flag, line, sample, cs, &range, NULL);
}


void PigPSPH::XYZtoLS(const PigPoint &xyz_in, const int infinity_flag,
		      double *line, double *sample, PigCoordSystem *cs,
                      double *range, double par[2][3]) const
{
    double ls[2];
    double pos[3];
    cmod_psph_t psph;
    //double range;

    if (!_psph_set) {
	printError("Attempt to use uninitialized PSPH camera model");
	return;
    }

    // If infinity, assume XYZ point is the look vector, and project the
    // vanishing point.  This should use the cmod code, but there is no
    // psph form of this function.  So, we just project a unit vector's
    // distance in front of the C point.

    PigPoint xyz;

    if (infinity_flag) {
	PigVector look = xyz_in;
	if (cs != _current_cs && cs != NULL) {
	    look = _current_cs->convertVector(look, cs);
	}
	xyz = _current_c + look;
    }
    else {
        xyz = xyz_in;
        if (cs != _current_cs && cs != NULL) {
	    xyz = _current_cs->convertPoint(xyz, cs);
        }
    }

    xyz.getXYZ(pos);

    fill_psph_t(&psph);

    cmod_psph_3d_to_2d(pos, &psph, ls, par);

    *sample = ls[0];
    *line = ls[1];
}

////////////////////////////////////////////////////////////////////////
// These functions should *ONLY* be called by the corresponding
// PointingModel!!  They generally mirror the similar functions in PM,
// but calling the PM's version allows the pointing model to do mission-
// specific pointing corrections or sanity checks.  See the comments
// in PigPointingModel for descriptions.  They are all returned in the
// current coordinate system.
//
// The Twist defined as is the angle between the AX vector and the Z axis,
// after rotating "A" to point down the X axis.
////////////////////////////////////////////////////////////////////////

double PigPSPH::getCameraTwist()
{
    if (!_psph_set) {
        printError("Attempt to use uninitialized PSPH camera model");
        return 0.0;
    }
    PigVector x_axis(1.0, 0.0, 0.0);
    PigVector y_axis(0.0, 1.0, 0.0);
    PigVector z_axis(0.0, 0.0, 1.0);

    // Determine quat for rotating "A" to match the X axis

    PigVector a = getCameraOrientation();
    PigVector axis = a * x_axis;
    axis.normalize();
    PigVector a_unit = a;
    a_unit.normalize();
    double angle = acos(a_unit % x_axis);
    PigQuaternion rot(axis, angle);

    // Rotate AX by that amount

    PigVector rotated_ax = rot * _current_ax;

    // Now determine angle of rotated AX with Z

    double result = acos(rotated_ax % z_axis);

    // Problem is, that gives the same answer for both positive and
    // negative twists.  If it's a positive twist, then rotated_ox cross z
    // will have a negative X component and vice-versa.

    PigVector sign = rotated_ax * z_axis;
    if (sign.getX() > 0)
	result = -result;

    return result;
}

////////////////////////////////////////////////////////////////////////
// Setting the Position is easy...
////////////////////////////////////////////////////////////////////////

void PigPSPH::setCameraPosition(PigPoint position, PigCoordSystem *cs)
{
    _current_c = _current_cs->convertPoint(position, cs);
}

////////////////////////////////////////////////////////////////////////
// Setting the Orientation maintains the same Twist.  This is done by
// compositing two rotations, "el" and "az", as the initial and final vectors
// for MoveCamera.  The third rotation ("twist") should remain invariant.
////////////////////////////////////////////////////////////////////////

void PigPSPH::setCameraOrientation(PigVector orientation, PigCoordSystem *cs)
{
    if (!_psph_set) {
        printError("Attempt to use uninitialized PSPH camera model");
	return;
    }

    if (cs != _current_cs)
	orientation = _current_cs->convertVector(orientation, cs);

    PigVector y_axis(0.0, 1.0, 0.0);
    PigVector z_axis(0.0, 0.0, 1.0);

    PigVector a = getCameraOrientation();

    // Composite rotation... el first, then az (quat multiplication is reversed)
    PigQuaternion rot1 = PigQuaternion(z_axis, _current_cs->getAz(a)) *
			 PigQuaternion(y_axis, _current_cs->getEl(a));
    PigQuaternion rot2 = PigQuaternion(z_axis, _current_cs->getAz(orientation))*
			 PigQuaternion(y_axis, _current_cs->getEl(orientation));

    moveCamera(_current_c, rot1, _current_c, rot2, _current_cs);
}

////////////////////////////////////////////////////////////////////////
// Do this the hard way, by computing the az/el for the current position,
// then creating a composite rotation including it and the initial and final
// twist values.  Note that Twist does not have a coordinate system because
// it is a rotation around the pointing direction so the CS doesn't matter.
////////////////////////////////////////////////////////////////////////

void PigPSPH::setCameraTwist(double twist)
{
    if (!_psph_set) {
        printError("Attempt to use uninitialized PSPH camera model");
        return;
    }

    PigVector x_axis(1.0, 0.0, 0.0);
    PigVector y_axis(0.0, 1.0, 0.0);
    PigVector z_axis(0.0, 0.0, 1.0);

    // Complete composite is twist, el, az... which means az * el * twist.

    PigVector a = getCameraOrientation();

    PigQuaternion azel = PigQuaternion(z_axis, _current_cs->getAz(a)) *
			 PigQuaternion(y_axis, _current_cs->getEl(a));

    PigQuaternion rot1 = azel * PigQuaternion(x_axis, getCameraTwist());
    PigQuaternion rot2 = azel * PigQuaternion(x_axis, twist);

    moveCamera(_current_c, rot1, _current_c, rot2, _current_cs);
}

/////////////////////////////////////////////////////////////////
// Given FOV angles in both line and sample directions,
// return LS min/max values. FOV angles should be 
// specified in degrees. min/max parameters are both input/output.  
// That way the implementation of this API can use input min/max
// to determine image size, though it's not that way here.  
//
// The idea behind this implementation is to take the "A" vector
// and rotate it in the plane of AX and AY by the specified FOV.
// These vectors are then projected through the camera model to
// get the min/max values.
/////////////////////////////////////////////////////////////////
void PigPSPH::getMinMaxLS(double fov_h, double fov_v,
			  double *x_min, double *x_max,
			  double *y_min, double *y_max) const
{

    double line, sample;
    PigVector axis, vector;
    PigPoint point;
    PigQuaternion *q;
    
    // Get rough equivalents to CAHV's A, H', and V' .  Note that Ax and Ay
    // are analogous to V' and H' respectively, but Ay points the opposite
    // direction.

    PigVector a = getCameraOrientation();
    PigVector hPrime = _current_ay * -1;
    PigVector vPrime = _current_ax;

    //Compute Sample max/min

    // axis of rotation
    axis = a * hPrime;

    // quaternion that defines rotation
    q = new PigQuaternion(axis, PigDeg2Rad(fov_h/2));
    // get the vector
    vector = (*q) * a;
    delete q;
    // make sure it's a unit vector
    vector.normalize();
    // Add to C to get a point in XYZ space
    point = _current_c + vector;
    XYZtoLS(point, 0, &line, &sample, _current_cs);

    *x_max = sample;

    // quaternion that defines rotation
    q = new PigQuaternion(axis, - PigDeg2Rad(fov_h/2));
    // get the vector
    vector = (*q) * a;
    delete q;
    // make sure it's a unit vector
    vector.normalize();
    // Add to C to get a point in XYZ space
    point = _current_c + vector;
    XYZtoLS(point, 0, &line, &sample, _current_cs);

    *x_min = sample;

    //Compute Line max/min

    // axis of rotation
    axis = a * vPrime;

    // quaternion that defines rotation
    q = new PigQuaternion(axis, PigDeg2Rad(fov_v/2));
    // get the vector
    vector = (*q) * a;
    delete q;
    // make sure it's a unit vector
    vector.normalize();
    // Add to C to get a point in XYZ space
    point = _current_c + vector;
    XYZtoLS(point, 0, &line, &sample, _current_cs);
    
    *y_max = line;

    // quaternion that defines rotation
    q = new PigQuaternion(axis, - PigDeg2Rad(fov_v/2));
    // get the vector
    vector = (*q) * a;
    delete q;
    // make sure it's a unit vector
    vector.normalize();
    // Add to C to get a point in XYZ space
    point = _current_c + vector;
    XYZtoLS(point, 0, &line, &sample, _current_cs);

    *y_min = line; 
}

/////////////////////////////////////////////////////////////////
// Interpolate this model based on the provided list.  Returns 1 on
// success, 0 on failure.
//
// Interpolation is not implemented for PSPH, so we simply set ourselves
// to match the first given model (assuming it's also PSPH).  Like the
// CAHV implementation, this assumes the camera model CS's match.
/////////////////////////////////////////////////////////////////

int PigPSPH::interpolateModels(int n_models, PigCameraModel *interp_models[],
                                double interp_values[], double value)
{
    if (n_models <= 0 || n_models > PIG_MAX_INTERP_CAMERA_MODELS)
	return 0;			// bad numbers

    if (strcasecmp(interp_models[0]->getModelName(), "PSPH") != 0)
	return 0;			// not a PSPH

    // Just copy the vectors from the first model

    cmod_psph_t psph;
    ((PigPSPH *)interp_models[0])->fill_psph_t(&psph);
    empty_psph_t(&psph);

    return 1;
}

/////////////////////////////////////////////////////////////////
// Convert to the generic cmod_t type.
// If we don't have a proper camera model size (xdim/ydim),
// we assume 1024.  That's right in most cases, and it doesn't really
// matter for the uses to which this is currently put (interpolation).
/////////////////////////////////////////////////////////////////

void PigPSPH::to_cmod_t(cmod_t *cmod)
{
    int nl = 1024;
    int ns = 1024;
    if (_dims_set) {
	nl = _ydim;
	ns = _xdim;
    }

    double c[3], ax[3], ay[3], nx[3], ny[3];

    _current_c.getXYZ(c);
    _current_ax.getXYZ(ax);
    _current_ay.getXYZ(ay);
    _current_nx.getXYZ(nx);
    _current_ny.getXYZ(ny);

    cmod_init_psph(ns, nl, c, ax, ay, nx, ny, _current_sx, _current_sy, cmod);
}

/////////////////////////////////////////////////////////////////
// Convert from the generic cmod_t type.  Requires that "this"s type
// match the cmod_t type, or an error is printed and returned
// (success==1, error==0).
/////////////////////////////////////////////////////////////////

int PigPSPH::from_cmod_t(cmod_t *cmod)
{
    if (cmod->mclass != CMOD_CLASS_PSPH) {
	printWarning("Invalid model type in from_cmod_t()");
	return 0;
    }
    setInitialPSPH(cmod->u.psph.c, cmod->u.psph.ax, cmod->u.psph.ay,
		   cmod->u.psph.nx, cmod->u.psph.ny,
		   cmod->u.psph.sx, cmod->u.psph.sy,
		   _initial_cs);

    return 1;

}

