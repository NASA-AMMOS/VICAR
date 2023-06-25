////////////////////////////////////////////////////////////////////////
// PigCAHV
//
// Implements the CAHV camera model.
////////////////////////////////////////////////////////////////////////

#include "PigCAHV.h"
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

#ifdef MAX
#undef MAX
#endif
#define MAX(a,b) (((a)>(b))?(a):(b))

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigCAHV::PigCAHV(const char *mission, const char *instrument,
		 const char *version, const char *subtype,
		 const char *construction, const char *calibration)
	: PigCameraModel(mission, instrument, version, 
			 subtype, construction, calibration)
{
    _cahv_set = FALSE;
}

PigCAHV::PigCAHV()
	: PigCameraModel()
{
    _cahv_set = FALSE;
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigCAHV::~PigCAHV()
{
    // nothing to do...
}

////////////////////////////////////////////////////////////////////////
// I/O routines
////////////////////////////////////////////////////////////////////////

int PigCAHV::readFromFile(const char *filename, PigCoordSystem *cs)
{
    double c[3], a[3], h[3], v[3];
    // The variables below are discarded after the read
    double s[12][12], hs, hc, vs, vc, theta, s_int[5][5];

    int status = cmod_cahv_read(filename, c, a, h, v, s,
				&hs, &hc, &vs, &vc, &theta, s_int);

    if (status != CAHVOR_SUCCESS)
	return status;		// SUCCESS is 0 for CAHV

    setInitialCAHV(c, a, h, v, cs);			// CS not changed

    return 0;
}

int PigCAHV::writeToFile(const char *filename) const
{
    double c[3], a[3], h[3], v[3];
    int xdim, ydim;
    double s[12][12], hs, hc, vs, vc, theta, s_int[5][5];
    char *comment = "Written from PigCAHV";

    _current_c.getXYZ(c);
    _current_a.getXYZ(a);
    _current_h.getXYZ(h);
    _current_v.getXYZ(v);
    cmod_cahv_internal(c,a,h,v, NULL, &hs, &hc, &vs, &vc, &theta, NULL);
    memset(s, 0, sizeof(s));
    memset(s_int, 0, sizeof(s_int));
    xdim = 1024;			//!!!! unknown by this class
    ydim = 1024;

    int status = cmod_cahv_write(filename, comment, c, a, h, v, s,
				hs, hc, vs, vc, theta, s_int);

    if (status != CAHVOR_SUCCESS)
	return status;		// SUCCESS is 0 for CAHV

    return 0;
}

////////////////////////////////////////////////////////////////////////
// The label routines expect a PigFileModel now.

int PigCAHV::readFromLabel(PigFileModel *file, int instance)
{
    const LblCameraModel_typ *CameraModel;
    PigCoordSystem *cs;
    PigCSReference *cs_ref;
    int status;

    CameraModel = file->getLblCameraModel(instance);
    if (CameraModel == NULL) {
        printError("Unable to read CAHV camera model from label!");
	return 1;
    }

    if (!CameraModel->ModelType.Valid) {
	printError("Missing ModelType in label");
	return 1;
    }
    if (strcasecmp(CameraModel->ModelType.Value, "CAHV") != 0) {
	printError("ModelType in label does not match subclass type of CAHV");
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

	   CameraModel->ModelComponent2[0].Valid &&         //A
	   CameraModel->ModelComponent2[1].Valid &&
	   CameraModel->ModelComponent2[2].Valid &&


	   CameraModel->ModelComponent3[0].Valid &&         //H
	   CameraModel->ModelComponent3[1].Valid &&
	   CameraModel->ModelComponent3[2].Valid &&


	   CameraModel->ModelComponent4[0].Valid &&         //V
	   CameraModel->ModelComponent4[1].Valid &&
	   CameraModel->ModelComponent4[2].Valid )) {
	printError("ModelComponent(s) not valid in label");
	return 1;
    }

    // Silly label API uses floats, not doubles!!  So we have to treat each
    // component individually

    PigVector c(CameraModel->ModelComponent1[0].Value,
		CameraModel->ModelComponent1[1].Value,
		CameraModel->ModelComponent1[2].Value);
    PigVector a(CameraModel->ModelComponent2[0].Value,
		CameraModel->ModelComponent2[1].Value,
		CameraModel->ModelComponent2[2].Value);
    PigVector h(CameraModel->ModelComponent3[0].Value,
		CameraModel->ModelComponent3[1].Value,
		CameraModel->ModelComponent3[2].Value);
    PigVector v(CameraModel->ModelComponent4[0].Value,
		CameraModel->ModelComponent4[1].Value,
		CameraModel->ModelComponent4[2].Value);

    setInitialCAHV(c,a,h,v, cs);

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

int PigCAHV::writeToLabel(int unit, int instance, PigCoordSystem *cs,
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

// Internal routine, used by subclasses to fill in basic CAHV info

void PigCAHV::setupWriteToLabel(LblCameraModel_typ &CM,PigCoordSystem *cs) const
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

    strcpy(CM.ModelType.Value, "CAHV");
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

    strcpy(CM.ModelComponentId[1].Value, "A");
    CM.ModelComponentId[1].Valid = 1;
    strcpy(CM.ModelComponentName[1].Value, "AXIS");
    CM.ModelComponentName[1].Valid = 1;
    strcpy(CM.ModelComponentUnit[1].Value, "N/A");
    CM.ModelComponentUnit[1].Valid = 1;
    PigVector a = cs->convertVector(_current_a, _current_cs);
    CM.ModelComponent2[0].Value = a.getX();
    CM.ModelComponent2[1].Value = a.getY();
    CM.ModelComponent2[2].Value = a.getZ();
    CM.ModelComponent2[0].Valid = 1;
    CM.ModelComponent2[1].Valid = 1;
    CM.ModelComponent2[2].Valid = 1;

    strcpy(CM.ModelComponentId[2].Value, "H");
    CM.ModelComponentId[2].Valid = 1;
    strcpy(CM.ModelComponentName[2].Value, "HORIZONTAL");
    CM.ModelComponentName[2].Valid = 1;
    strcpy(CM.ModelComponentUnit[2].Value, "PIXEL");
    CM.ModelComponentUnit[2].Valid = 1;
    PigVector h = cs->convertVector(_current_h, _current_cs);
    CM.ModelComponent3[0].Value = h.getX();
    CM.ModelComponent3[1].Value = h.getY();
    CM.ModelComponent3[2].Value = h.getZ();
    CM.ModelComponent3[0].Valid = 1;
    CM.ModelComponent3[1].Valid = 1;
    CM.ModelComponent3[2].Valid = 1;

    strcpy(CM.ModelComponentId[3].Value, "V");
    CM.ModelComponentId[3].Valid = 1;
    strcpy(CM.ModelComponentName[3].Value, "VERTICAL");
    CM.ModelComponentName[3].Valid = 1;
    strcpy(CM.ModelComponentUnit[3].Value, "PIXEL");
    CM.ModelComponentUnit[3].Valid = 1;
    PigVector v = cs->convertVector(_current_v, _current_cs);
    CM.ModelComponent4[0].Value = v.getX();
    CM.ModelComponent4[1].Value = v.getY();
    CM.ModelComponent4[2].Value = v.getZ();
    CM.ModelComponent4[0].Valid = 1;
    CM.ModelComponent4[1].Valid = 1;
    CM.ModelComponent4[2].Valid = 1;

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
// Read the CM from a string.  For CAHV, only the CAHV vectors themselves
// are read.  Any error will return 1.  The parser is not very forgiving.

int PigCAHV::readFromString(const char *string, PigCoordSystem *cs)
{
    if (cs == NULL)
	cs = _initial_cs;

    double c[3], a[3], h[3], v[3];

    int status = sscanf(string,
	"CAHV: C=(%lf,%lf,%lf) A=(%lf,%lf,%lf) H=(%lf,%lf,%lf) V=(%lf,%lf,%lf)",
		&c[0], &c[1], &c[2], &a[0], &a[1], &a[2],
		&h[0], &h[1], &h[2], &v[0], &v[1], &v[2]);
    if (status != 12)		// 12 items to read
	return 1;

    setInitialCAHV(c, a, h, v, cs);

    return 0;
}

////////////////////////////////////////////////////////////////////////
// Write the CM to a string.  For CAHV, only the CAHV vectors themselves
// are written.

int PigCAHV::writeToString(char *string, int max_length)
{
    char buf[255];

    sprintf(buf,
	"CAHV: C=(%lf,%lf,%lf) A=(%lf,%lf,%lf) H=(%lf,%lf,%lf) V=(%lf,%lf,%lf)",
		_current_c.getX(), _current_c.getY(), _current_c.getZ(),
		_current_a.getX(), _current_a.getY(), _current_a.getZ(),
		_current_h.getX(), _current_h.getY(), _current_h.getZ(),
		_current_v.getX(), _current_v.getY(), _current_v.getZ());
    if (strlen(buf) + 1 >= max_length)
	return 1;			// whoops, not big enough
    strcpy(string, buf);

    return 0;
}

////////////////////////////////////////////////////////////////////////
// Set the coordinate systems
////////////////////////////////////////////////////////////////////////

void PigCAHV::setInitialCoordSystem(PigCoordSystem *cs)
{
    if (_cahv_set && cs != _initial_cs) {	// change initial params
	_orig_c = cs->convertPoint(_orig_c, _initial_cs);
	_orig_a = cs->convertVector(_orig_a, _initial_cs);
	_orig_h = cs->convertVector(_orig_h, _initial_cs);
	_orig_v = cs->convertVector(_orig_v, _initial_cs);
    }
    _initial_cs = cs;
    setCoordSystem(cs);
}

void PigCAHV::setCoordSystem(PigCoordSystem *cs)
{
    _current_c = cs->convertPoint(_current_c, _current_cs);
    _current_a = cs->convertVector(_current_a, _current_cs);
    _current_h = cs->convertVector(_current_h, _current_cs);
    _current_v = cs->convertVector(_current_v, _current_cs);

    _current_cs = cs;
}

////////////////////////////////////////////////////////////////////////
// Set the initial CAHV vectors.  Note that the cs argument defines
// what the given vectors are measured in... it does NOT change
// the coordinate system definition of the object.  To do that, callers
// must set the CS first, then set the CAHV vectors.
////////////////////////////////////////////////////////////////////////

void PigCAHV::setInitialCAHV(const PigPoint c, const PigVector a,
				const PigVector h, const PigVector v,
				PigCoordSystem *cs)
{
    _orig_c = _initial_cs->convertPoint(c, cs);
    _orig_a = _initial_cs->convertVector(a, cs);
    _orig_h = _initial_cs->convertVector(h, cs);
    _orig_v = _initial_cs->convertVector(v, cs);
    _current_c = _current_cs->convertPoint(c, cs);
    _current_a = _current_cs->convertVector(a, cs);
    _current_h = _current_cs->convertVector(h, cs);
    _current_v = _current_cs->convertVector(v, cs);

    _cahv_set = TRUE;
}

void PigCAHV::setInitialCAHV(const double c[3], const double a[3],
				const double h[3], const double v[3],
				PigCoordSystem *cs)
{
    PigPoint cc(c);
    PigVector aa(a);
    PigVector hh(h);
    PigVector vv(v);
    setInitialCAHV(cc, aa, hh, vv, cs);
}

////////////////////////////////////////////////////////////////////////
// Set the current CAHV vectors.  Note that the cs argument defines
// what the given vectors are measured in... it does NOT change
// the coordinate system definition of the object.  To do that, callers
// must set the CS first, then set the CAHV vectors.
////////////////////////////////////////////////////////////////////////

void PigCAHV::setCurrentCAHV(const PigPoint c, const PigVector a,
				const PigVector h, const PigVector v,
				PigCoordSystem *cs)
{
    _current_c = _current_cs->convertPoint(c, cs);
    _current_a = _current_cs->convertVector(a, cs);
    _current_h = _current_cs->convertVector(h, cs);
    _current_v = _current_cs->convertVector(v, cs);
}

void PigCAHV::setCurrentCAHV(const double c[3], const double a[3],
				const double h[3], const double v[3],
				PigCoordSystem *cs)
{
    PigPoint cc(c);
    PigVector aa(a);
    PigVector hh(h);
    PigVector vv(v);
    setCurrentCAHV(cc, aa, hh, vv, cs);
}

////////////////////////////////////////////////////////////////////////
// Get the current CAHV vectors
////////////////////////////////////////////////////////////////////////

void PigCAHV::getCurrentCAHV(PigPoint &c, PigVector &a,
			     PigVector &h, PigVector &v)
{
    c = _current_c;
    a = _current_a;
    h = _current_h;
    v = _current_v;
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

void PigCAHV::moveCamera(const PigPoint initial_point,
			 const PigQuaternion initial_orientation,
			 const PigPoint final_point,
			 const PigQuaternion final_orientation,
			 PigCoordSystem *cs)
{
    double p_i[3], q_i[4];
    double p_f[3], q_f[4];
    double c_i[3], a_i[3], h_i[3], v_i[3];
    double c_f[3], a_f[3], h_f[3], v_f[3];

    if (!_cahv_set) {
	printError("Attempt to modify uninitialized CAHV camera model");
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

    _current_c.getXYZ(c_i);
    _current_a.getXYZ(a_i);
    _current_h.getXYZ(h_i);
    _current_v.getXYZ(v_i);

    cmod_cahv_move(p_i, q_i, c_i, a_i, h_i, v_i,
				   p_f, q_f, c_f, a_f, h_f, v_f);

    _current_c.setXYZ(c_f);
    _current_a.setXYZ(a_f);
    _current_h.setXYZ(h_f);
    _current_v.setXYZ(v_f);

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

void PigCAHV::scaleCamera(const double x_factor, 
			  const double y_factor)
{
    double a_i[3], h_i[3], v_i[3];
    double h_f1[3], v_f1[3];
    double h_f2[3], v_f2[3];
    double h_f[3], v_f[3];
  
    _current_a.getXYZ(a_i);
    _current_h.getXYZ(h_i);
    _current_v.getXYZ(v_i);
 
    cmod_cahv_shift(-0.5, -0.5, a_i, h_i, v_i, NULL, h_f1, v_f1, NULL);
    cmod_cahv_scale(x_factor, y_factor, h_f1, v_f1, NULL, h_f2, v_f2, NULL);
    cmod_cahv_shift(0.5, 0.5, a_i, h_f2, v_f2, NULL, h_f, v_f, NULL);

    _current_h.setXYZ(h_f);
    _current_v.setXYZ(v_f);

}

///////////////////////////////////////////////////////////////////////
// This function shifts a camera model. The shift values should be
// understood as the coordinates of the original model where the origin
// will fall in the new one.  In another words first_line, first_sample
// is the start of the image w.r.t. the Full Frame.
///////////////////////////////////////////////////////////////////////

void PigCAHV::shiftCamera(const double dx, 
			  const double dy)
{
    double a_i[3];
    double h_i[3], v_i[3];
    double h_f[3], v_f[3];

    _current_a.getXYZ(a_i);  
    _current_h.getXYZ(h_i);
    _current_v.getXYZ(v_i);
 
    cmod_cahv_shift(dx, dy, a_i, h_i, v_i, NULL, h_f, v_f, NULL);

    _current_h.setXYZ(h_f);
    _current_v.setXYZ(v_f);
}

////////////////////////////////////////////////////////////////////////
// Aligns the model with another for use with stereo imagery.
//
// This method works with ANY of the CAHV* models as input, so it should
// not be subclassed.
//
// The output model matches "this" model.  Call alignStereoCameras on the
// other camera model with this as a parameter to get the aligned partner.
//
// The input models are unchanged. The output model retains the location
// of the original, but will be pointing parallel to the (aligned) partner
// model, at the "average" look direction of the two.  This essentially
// removes toe-in and other nasty physical non-alignments of stereo
// cameras, giving a nice, parallel look direction to both cameras.
// It should also be able to "align" pictures taken from the same camera
// at different times, or wildly different look angles.
//
// If there is no stereo partner, or the partner is not CAHV*, or if the
// two cameras are coincident, then this model is simply warped to a linear
// model and returned.  NULL is returned if something really bad happens.
//
// Note: we linearize (separately) both the Initial and Current values, so
// that we can point the camera either before or after.  For the stereo
// partner (Actual lin) case, you really want to point before, and not
// repoint after linearization.  For the synthesized partner (nominal lin)
// case, you can go either way.
//
// x/ydim_in1 refers to "this" model, x/ydim_in2 refers to "other".
// If the "other" size is not known, pass in the same for 1 and 2.
//
// Three parameters in POINT_METHOD control the behavior:
//
// CMOD_WARP=n
// Sets the algorithm used for warping.  Current values:
//   1 = old mode, used by MER and MSL since 2012
//   2 = new model (2017), used by InSight and Mars 2020
//   3 or PSPH = aligns to (and returns) PSPH model instead of CAHV
// Note that algorithm 2 is much better for nontraditional stereo, such as
// vertical or other weird geometries.  CAHV supports only 1, CAHVOR supports
// only 1 and 2, CAHVOR supports all three.
//
// CAHV_FOV=x
// Sets the field of view for the output cameras.
//   MIN or INTERSECT: (default) Picks the FOV as the intersection of the
//	intput cameras.  The output image will be missing a (sometimes
//	significant) portion of the overlap area, but there are no black
//	areas; the entire image overlaps its partner.
//   MAX or UNION: Picks the FOV as the union of the two input cameras.
//      Thus, there are black areas on the side, but all overlapping pixels
//	are preserved.  Depending on camera geometry, significant resolution
//	may be lost, especially for fisheye cameras, due to the black areas.
//   LINEAR:  Uses only CAHV vectors, ignoring higher order (O,R,E) terms.
//	This has the advantage of best preserving horizontal aspect ratio
//	and results in images that are similar scale-wise to the original.
//	It usually ends up in between MAX and MIN as a compromise.
//	Note that there is no choice of CMOD_WARP algorithm for LINEAR mode.
//
// FORCE_LIN=x
// Forces the camera model type to be used for linearization.  In reality
// there is little need for this.  However, since the default is (as of Feb
// 2019) to change CAHV to CAHVOR in order to use a version of warp_models
// that is aware of the image FOV, we allow an option to restore the old
// behavior (there are no CAHV stereo pairs in active missions).
//   CAHV: force the model to CAHV
//   CAHVOR: force the model to CAHVOR
//   CAHVORE: force the model to CAHVORE
//   ALLOW_CAHV: don't force CAHV's to CAHVOR
//   default: select CAHVOR or CAHVORE as needed; force CAHV to CAHVOR.
////////////////////////////////////////////////////////////////////////

PigCameraModel *PigCAHV::alignStereoCameras(int xdim_in1, int ydim_in1,
					    int xdim_in2, int ydim_in2,
					    int xdim_out, int ydim_out,
					    PigCameraModel *other)
{
    double c1[3], a1[3], h1[3], v1[3], o1[3], r1[3], e1[3];
    double ci1[3], ai1[3], hi1[3], vi1[3], oi1[3], ri1[3], ei1[3];
    int t1;
    double p1;
    PigCmodType model1;
    double c2[3], a2[3], h2[3], v2[3], o2[3], r2[3], e2[3];
    double ci2[3], ai2[3], hi2[3], vi2[3], oi2[3], ri2[3], ei2[3];
    int t2;
    double p2;
    double ao[3], ho[3], vo[3];
    PigCmodType model2;
    PigCameraModel *cam2 = NULL;
    PigCAHV *outputModel = NULL;
    double hs, hc, vs, vc, theta;
    int idims[2], odims[2];
    int min_fov = MIN_FOV;
    int linear = 0;

    // Gather params and such

    // cmod_cahvor_warp_models() (at least) crashes with images of size 1xn
    // or mx1.  Since this is a degenerate case anyway, make the size 2 to
    // avoid the assert.

    if (xdim_in1 == 1) xdim_in1 = 2;
    if (ydim_in1 == 1) ydim_in1 = 2;
    if (xdim_in2 == 1) xdim_in2 = 2;
    if (ydim_in2 == 1) ydim_in2 = 2;
    if (xdim_out == 1) xdim_out = 2;
    if (ydim_out == 1) ydim_out = 2;

    // The warp_models routines are inconsistent in how they handle image
    // dimensions.  Some take 1 input, some take 2, and some take dims in
    // arrays rather than separate parameters.  Set up the arrays for those
    // that need it.  For those that take only one image, we use the max of
    // the two inputs so the operation will be "commutative" (i.e. warp(a,b)
    // and warp (b,a) give models aligned to each other).
    idims[0] = MAX(xdim_in1, xdim_in2);
    idims[1] = MAX(ydim_in1, ydim_in2);
    odims[0] = xdim_out;
    odims[1] = ydim_out;

    // If the image sizes aren't the same, then force the cahvore version
    // of warp_models.  That way we do the right thing even for cahvor.

    int force_cahvore = FALSE;
    if (xdim_in1 != xdim_in2 || ydim_in1 != ydim_in2)
	force_cahvore = TRUE;

    PigCmodWarpAlgorithm warp_algo = getCmodWarpAlgorithm();

    // Get the pointing model name to use
    char point_method[1024];
    int count;
    char *pointMethodValue = NULL;
    getParam("POINT_METHOD", point_method, &count, 1, 0);
    if (count == 1)
        pointMethodValue = parseParamString(point_method, "CAHV_FOV");
   
    if (pointMethodValue) {
        if (!strncasecmp(pointMethodValue, "MAX", 3) ||
            !strncasecmp(pointMethodValue, "UNION", 5))
            min_fov = 0;
        else if (!strncasecmp(pointMethodValue, "MIN", 3) ||
            !strncasecmp(pointMethodValue, "INTERSECT", 9))
            min_fov = 1;
        else if (!strncasecmp(pointMethodValue, "LINEAR", 6))
            linear = 1;
    }

    // Get full set of vectors for this model

    get_initial_cahvore_vectors(
		ci1, ai1, hi1, vi1, oi1, ri1, ei1, t1, p1, model1, _initial_cs);
    get_cahvore_vectors(
		c1, a1, h1, v1, o1, r1, e1, t1, p1, model1, _current_cs);

    // Check if stereo partner is supplied and is a CAHV*

    int no_partner = FALSE;

    if (other == NULL || strncasecmp(other->getModelName(), "CAHV", 4) != 0) {
	no_partner = TRUE;
    }
    else {

	// CAHV* model, so get all the vectors and check.  We clone the
	// model so we can change the coord sys without affecting the original.

	cam2 = other->clone();
	cam2->setCoordSystem(_current_cs);

        other->get_initial_cahvore_vectors(
		ci2, ai2, hi2, vi2, oi2, ri2, ei2, t2, p2, model2, _initial_cs);
        other->get_cahvore_vectors(
		c2, a2, h2, v2, o2, r2, e2, t2, p2, model2, _current_cs);

	delete cam2;

	// If the C's of the two models are equal, the camera models are
	// deemed to be the same.  Thus we don't have a stereo partner.

	if ((c1[0] == c2[0] && c1[1] == c2[1] && c1[2] == c2[2]) ||
	    (ci1[0]==ci2[0] && ci1[1]==ci2[1] && ci1[2]==ci2[2])) {
	    no_partner = TRUE;
	}
    }

    // Create the output model assuming it's CAHV.  If we actually end up
    // using PSPH, it will be deleted and re-created.  Slightly wasteful,
    // but much cleaner coding.

    outputModel = new PigCAHV(_mission, _instrument, _version,
				_subtype, _construction, _calibration);
    outputModel->setInitialCoordSystem(_initial_cs);
    outputModel->setCoordSystem(_current_cs);
    transferMetadata(outputModel);

    // If we don't have a partner, it's not a CAHV, or the C's match, then
    // simply linearize the model we have.  We probably could use the
    // cahvore_warp_model for all cases, but to be safe we call the correct
    // version based on our type.
    // HOWEVER... if the input and output sizes are not the same then we use
    // the CAHVORE version to get resizing (else marsmcauley zoom doesn't work).

    if (no_partner) {

	if (xdim_in1 != xdim_out || ydim_in1 != ydim_out)
	    model1 = CMOD_TYPE_CAHVORE;

	switch (model1) {
	    case CMOD_TYPE_CAHV:
		outputModel->setInitialCAHV(ci1, ai1, hi1, vi1, _initial_cs);
		outputModel->setCurrentCAHV(c1, a1, h1, v1, _current_cs);
		return outputModel;

	    case CMOD_TYPE_CAHVOR:
		outputModel->setInitialCAHV(ci1, ai1, hi1, vi1, _initial_cs);
		cmod_cahvor_warp_model(ci1, ai1, hi1, vi1, oi1, ri1,
				min_fov, idims, odims,
				ao, ho, vo,
				&hs,  &hc, &vs, &vc, &theta);
		outputModel->setInitialCAHV(ci1, ao, ho, vo, _initial_cs);
		cmod_cahvor_warp_model(c1, a1, h1, v1, o1, r1,
				min_fov, idims, odims,
				ao, ho, vo,
				&hs,  &hc, &vs, &vc, &theta);
		outputModel->setCurrentCAHV(c1, ao, ho, vo, _current_cs);
		return outputModel;

	    case CMOD_TYPE_CAHVORE:
		cmod_cahvore_warp_model(MAX(xdim_in1,xdim_in2),
					MAX(ydim_in1, ydim_in2), t1, p1,
					ci1, ai1, hi1, vi1, oi1, ri1, ei1,
					LIM_FOV, min_fov, xdim_out, ydim_out,
					ao, ho, vo,
					&hs, &hc, &vs, &vc, &theta);
		outputModel->setInitialCAHV(ci1, ao, ho, vo, _initial_cs);
		cmod_cahvore_warp_model(MAX(xdim_in1,xdim_in2),
					MAX(ydim_in1, ydim_in2), t1, p1,
					c1, a1, h1, v1, o1, r1, e1,
					LIM_FOV, min_fov, xdim_out, ydim_out,
					ao, ho, vo,
					&hs, &hc, &vs, &vc, &theta);
		outputModel->setCurrentCAHV(c1, ao, ho, vo, _current_cs);
		return outputModel;

	    default:
		printFatal("Internal error in alignStereo (no_partner)");
		break;
	}
	return NULL;
    }

    // At this point we know we have a valid partner.  Use the warp_models
    // version that's the "highest" one of either input.

    PigCmodType which_model = MAX(model1, model2);

    char *force_lin = NULL;
    if (count == 1)			// still from POINT_METHOD
        force_lin = parseParamString(point_method, "FORCE_LIN");

    // Parse out FORCE_LIN.  If it's one of CAHV, CAHVOR, or CAHVORE, force to
    // that.  Otherwise, if it's *not* ALLOW_CAHV, then force any CAHV's to
    // CAHVOR so we use an image-size-aware version of warp_models.

    if (force_lin && !strncasecmp(force_lin,"CAHVORE",7))
	which_model = CMOD_TYPE_CAHVORE;
    else if (force_lin && !strncasecmp(force_lin,"CAHVOR",6))
	which_model = CMOD_TYPE_CAHVOR;
    else if (force_lin && !strncasecmp(force_lin,"CAHV",4))
	which_model = CMOD_TYPE_CAHV;
    else if (!(force_lin && !strncasecmp(force_lin,"ALLOW_CAHV",10))) {
	if (which_model == CMOD_TYPE_CAHV)
	    which_model = CMOD_TYPE_CAHVOR;
    }

    // Special case: if mode LINEAR is set, then we use the CAHV warp
    // regardless of what the input types are...

    if (linear)
	which_model = CMOD_TYPE_CAHV;

    // Another special case: if the input images don't match, force to the
    // CAHVORE version, which supports this

    if (force_cahvore && !linear)
	which_model = CMOD_TYPE_CAHVORE;

    switch (which_model) {

	case CMOD_TYPE_CAHV:

	    // There is only one warp_models for cahv so no need to check
	    // the algorithm.

	    cmod_cahv_warp_models(ci1, ai1, hi1, vi1, ci2, ai2, hi2, vi2,
			ao, ho, vo, &hs, &hc, &vs, &vc, &theta);
	    outputModel->setInitialCAHV(ci1, ao, ho, vo, _initial_cs);
	    cmod_cahv_warp_models(c1, a1, h1, v1, c2, a2, h2, v2,
			ao, ho, vo, &hs, &hc, &vs, &vc, &theta);
	    outputModel->setCurrentCAHV(c1, ao, ho, vo, _current_cs);

	    return outputModel;

	case CMOD_TYPE_CAHVOR:

	    // Note that PSPH is not available for CAHVOR.  If you asked for
	    // it, drop down to 2 since the PSPH linearization is based on 2.

	    if (warp_algo == CMOD_WARP_1) {
		cmod_cahvor_warp_models(ci1, ai1, hi1, vi1, oi1, ri1,
					ci2, ai2, hi2, vi2, oi2, ri2,
					min_fov, idims, odims,
					ao, ho, vo,
					&hs, &hc, &vs, &vc, &theta);
	        outputModel->setInitialCAHV(ci1, ao, ho, vo, _initial_cs);
		cmod_cahvor_warp_models(c1, a1, h1, v1, o1, r1,
					c2, a2, h2, v2, o2, r2,
					min_fov, idims, odims,
					ao, ho, vo,
					&hs, &hc, &vs, &vc, &theta);
	        outputModel->setCurrentCAHV(c1, ao, ho, vo, _current_cs);
	    } else {
		cmod_cahvor_warp_models2(ci1, ai1, hi1, vi1, oi1, ri1,
					ci2, ai2, hi2, vi2, oi2, ri2,
					min_fov, idims, odims,
					ao, ho, vo,
					&hs, &hc, &vs, &vc, &theta);
	        outputModel->setInitialCAHV(ci1, ao, ho, vo, _initial_cs);
		cmod_cahvor_warp_models2(c1, a1, h1, v1, o1, r1,
					c2, a2, h2, v2, o2, r2,
					min_fov, idims, odims,
					ao, ho, vo,
					&hs, &hc, &vs, &vc, &theta);
	        outputModel->setCurrentCAHV(c1, ao, ho, vo, _current_cs);
	    }

	    return outputModel;

	case CMOD_TYPE_CAHVORE:

	    if (warp_algo == CMOD_WARP_1) {

		cmod_cahvore_warp_models(xdim_in1, ydim_in1, t1, p1,
					ci1, ai1, hi1, vi1, oi1, ri1, ei1,
					xdim_in2, ydim_in2, t2, p2,
					ci2, ai2, hi2, vi2, oi2, ri2, ei2,
					LIM_FOV, min_fov, xdim_out, ydim_out,
					ao, ho, vo,
					&hs, &hc, &vs, &vc, &theta);
		outputModel->setInitialCAHV(ci1, ao, ho, vo, _initial_cs);
		cmod_cahvore_warp_models(xdim_in1, ydim_in1, t1, p1,
					c1, a1, h1, v1, o1, r1, e1,
					xdim_in2, ydim_in2, t2, p2,
					c2, a2, h2, v2, o2, r2, e2,
					LIM_FOV, min_fov, xdim_out, ydim_out,
					ao, ho, vo,
					&hs, &hc, &vs, &vc, &theta);
		outputModel->setCurrentCAHV(c1, ao, ho, vo, _current_cs);
		return outputModel;

	    } else if (warp_algo == CMOD_WARP_2) {

		cmod_cahvore_warp_models2(xdim_in1, ydim_in1, t1, p1,
					ci1, ai1, hi1, vi1, oi1, ri1, ei1,
					xdim_in2, ydim_in2, t2, p2,
					ci2, ai2, hi2, vi2, oi2, ri2, ei2,
					LIM_FOV, min_fov, xdim_out, ydim_out,
					ao, ho, vo,
					&hs, &hc, &vs, &vc, &theta);
		outputModel->setInitialCAHV(ci1, ao, ho, vo, _initial_cs);
		cmod_cahvore_warp_models2(xdim_in1, ydim_in1, t1, p1,
					c1, a1, h1, v1, o1, r1, e1,
					xdim_in2, ydim_in2, t2, p2,
					c2, a2, h2, v2, o2, r2, e2,
					LIM_FOV, min_fov, xdim_out, ydim_out,
					ao, ho, vo,
					&hs, &hc, &vs, &vc, &theta);
		outputModel->setCurrentCAHV(c1, ao, ho, vo, _current_cs);
		return outputModel;

	    } else if (warp_algo == CMOD_WARP_PSPH) {

		// Destroy the CAHV model and re-create a PSPH one

		delete outputModel;

		PigPSPH *outputModelP;
		outputModelP = new PigPSPH(_mission, _instrument, _version,
					_subtype, _construction, _calibration);
		outputModelP->setInitialCoordSystem(_initial_cs);
		outputModelP->setCoordSystem(_current_cs);
		transferMetadata(outputModel);

		cmod_psph_t psph1, psph2;

		// Vergence param is always 0 for now per Todd Litwin

		cmod_cahvore_warp_models3(xdim_in1, ydim_in1, t1, p1,
					ci1, ai1, hi1, vi1, oi1, ri1, ei1,
					xdim_in2, ydim_in2, t2, p2,
					ci2, ai2, hi2, vi2, oi2, ri2, ei2,
					LIM_FOV, min_fov, 0, xdim_out, ydim_out,
					&psph1, &psph2);
		outputModelP->setInitialPSPH(psph1.c, psph1.ax, psph1.ay,
					    psph1.nx, psph1.ny,
					    psph1.sx, psph1.sy, _initial_cs);
		cmod_cahvore_warp_models3(xdim_in1, ydim_in1, t1, p1,
					c1, a1, h1, v1, o1, r1, e1,
					xdim_in2, ydim_in2, t2, p2,
					c2, a2, h2, v2, o2, r2, e2,
					LIM_FOV, min_fov, 0, xdim_out, ydim_out,
					&psph1, &psph2);
		outputModelP->setCurrentPSPH(psph1.c, psph1.ax, psph1.ay,
					    psph1.nx, psph1.ny,
					    psph1.sx, psph1.sy, _current_cs);
		return outputModelP;
	    } else {
		printFatal("Internal error in alignCameras(bad warp)");
		return NULL;
	    }

	default:
	    printFatal("Internal error in alignStereo (model type)");
	    break;
    }

    return NULL;
}

////////////////////////////////////////////////////////////////////////
// Resets the camera pointing to the "reference" location (usually
// where the calibration was taken), in preparation for a new pointing.
// This resets the coordinate system as well.
////////////////////////////////////////////////////////////////////////

void PigCAHV::resetCameraLocation()
{
    _current_c = _orig_c;
    _current_a = _orig_a;
    _current_h = _orig_h;
    _current_v = _orig_v;
    _current_cs = _initial_cs;
}

////////////////////////////////////////////////////////////////////////
// Copy the Current parameters to the Initial ("reference") model
// that is used by resetCameraLocation().  This is often used to
// modify the initial model for downsample/subframe before pointing
// takes place.
////////////////////////////////////////////////////////////////////////

void PigCAHV::setInitialFromCurrent()
{
    _orig_c = _current_c;
    _orig_a = _current_a;
    _orig_h = _current_h;
    _orig_v = _current_v;
    _initial_cs = _current_cs;
}

////////////////////////////////////////////////////////////////////////
// Retrieves the image-space coordinates (LS below) of the "pointing
// axis" of the camera.  The 0,0 point is encoded in the H and V vectors
// (basically, it is the origin of the image plane during calibration).
// This can be used to relate "logical" LS coordinates to the physical
// coordinates of a file.  Coordinates are assumed to be 0-based (unlike
// VICAR files).
////////////////////////////////////////////////////////////////////////

void PigCAHV::getCameraCenter(double &line, double &sample) const
{
    if (!_cahv_set) {
	printError("Attempt to use uninitialized CAHV camera model");
	return;
    }

    line = _current_a % _current_v;		// dot product
    sample = _current_a % _current_h;
}

////////////////////////////////////////////////////////////////////////
// Returns the angle subtended by a pixel, in either the line or sample
// directions (normally these will be the same).  The angle is in radians,
// so the units are radians/pixel.  If this is not constant across the
// camera, the angle for the central pixel (looking down the pointing
// vector) is returned.  If "which" is 0, the Line direction is returned;
// 1 returns the Sample direction.
//
// Derivation:  H is defined as f*H' + i0*A (see CAHV paper).  We derive
// f (focal length) and then construct a right triangle with f as the long
// side and 1.0 (pixels on the focal plane) as the short side.  The arctan
// of those is thus the angle subtended by that one pixel.
////////////////////////////////////////////////////////////////////////

double PigCAHV::getPixelAngle(const int which) const
{
    double f, ang;

    if (!_cahv_set) {
	printError("Attempt to use uninitialized CAHV camera model");
	return 0.0;
    }
    if (which == 0) {				// Line (v) direction
	f = (_current_v - _current_a * (_current_v % _current_a)).magnitude();
	ang = atan(1.0 / f);
    }
    else {
	f = (_current_h - _current_a * (_current_h % _current_a)).magnitude();
	ang = atan(1.0 / f);
    }
    return ang;
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

void PigCAHV::LStoLookVector(const double line, const double sample,
			PigPoint &origin, PigVector &look_direction,
			PigCoordSystem *cs) const
{
    double ls[2];
    double pos[3], uvec[3];
    double c[3], a[3], h[3], v[3];

    if (!_cahv_set) {
	printError("Attempt to use uninitialized CAHV camera model");
	return;
    }

    ls[0] = sample;
    ls[1] = line;

    _current_c.getXYZ(c);
    _current_a.getXYZ(a);
    _current_h.getXYZ(h);
    _current_v.getXYZ(v);

    cmod_cahv_2d_to_3d(ls, c, a, h, v, pos, uvec, NULL);

    origin.setXYZ(pos);
    look_direction.setXYZ(uvec);

    if (cs != _current_cs && cs != NULL) {
	origin = cs->convertPoint(origin, _current_cs);
	look_direction = cs->convertVector(look_direction, _current_cs);
    }
}


void PigCAHV::XYZtoLS(const PigPoint &xyz_in, const int infinity_flag,
	              double *line, double *sample, PigCoordSystem *cs) const
{
   double range;
   XYZtoLS(xyz_in, infinity_flag, line, sample, cs, &range, NULL);   
}



void PigCAHV::XYZtoLS(const PigPoint &xyz_in, const int infinity_flag,
		      double *line, double *sample, PigCoordSystem *cs,
                      double *range, double par[2][3]) const
{
    double ls[2];
    double pos[3];
    double c[3], a[3], h[3], v[3];
    //double range;

    if (!_cahv_set) {
	printError("Attempt to use uninitialized CAHV camera model");
	return;
    }

    // If infinity, assume XYZ point is the look vector, and project the
    // vanishing point.  This should use the cmod_cahv code, but a)
    // cmod_cahv_3d_to_2d_ray() calculates a bunch of stuff we don't need
    // (the back projection of the ray) and this is fairly time-crtitical
    // code, anb b) there appears to be no cahvor form of this function.
    // Note that for the infinity case, the CS conversion needs to use
    // the vector form, not the point form!

    if (infinity_flag) {
	PigVector look = xyz_in;
	if (cs != _current_cs && cs != NULL) {
	    look = _current_cs->convertVector(look, cs);
	}
	double x = look % _current_a;
	*sample = (look % _current_h) / x;
	*line = (look % _current_v) / x;
	return;
    }

    PigPoint xyz = xyz_in;
    if (cs != _current_cs && cs != NULL) {
	xyz = _current_cs->convertPoint(xyz, cs);
    }

    xyz.getXYZ(pos);

    _current_c.getXYZ(c);
    _current_a.getXYZ(a);
    _current_h.getXYZ(h);
    _current_v.getXYZ(v);

//!!!! What about range parameter????!!!!

   // cmod_cahv_3d_to_2d(pos, c, a, h, v, &range, ls, NULL);
    cmod_cahv_3d_to_2d(pos, c, a, h, v, range, ls, par);

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
// The Twist defined as is the angle between the X-V' plane and the Z axis,
// after rotating A to point down the X axis.
////////////////////////////////////////////////////////////////////////

double PigCAHV::getCameraTwist()
{
    if (!_cahv_set) {
        printError("Attempt to use uninitialized CAHV camera model");
        return 0.0;
    }
    PigVector x_axis(1.0, 0.0, 0.0);
    PigVector y_axis(0.0, 1.0, 0.0);
    PigVector z_axis(0.0, 0.0, 1.0);

    // Determine quat for rotating A to match the X axis

    PigVector axis = _current_a * x_axis;
    axis.normalize();
    PigVector a_unit = _current_a;
    a_unit.normalize();
    double angle = acos(a_unit % x_axis);
    PigQuaternion rot(axis, angle);

    // Determine V'

    PigVector v_prime = _current_v - _current_a * (_current_a % _current_v);

    // Rotate V' by that amount

    PigVector rotated_v = rot * v_prime;

    // Now determine angle of rotated V',X plane with Z
    // Make sure we return a full range of -180..180 deg (in rads)

    PigVector n = (rotated_v * x_axis);	// perp. to v''-x plane (like h')
    n.normalize();
    double result = asin(n % z_axis);		// really 90-arccos(n%z_axis)
    if ((n % y_axis) < 0) {
	result = PigDeg2Rad(180) - result;
	if (result > PigDeg2Rad(180))
	    result -= PigDeg2Rad(360);
    }
    return result;
}

////////////////////////////////////////////////////////////////////////
// Setting the Position is easy...
////////////////////////////////////////////////////////////////////////

void PigCAHV::setCameraPosition(PigPoint position, PigCoordSystem *cs)
{
    _current_c = _current_cs->convertPoint(position, cs);
}

////////////////////////////////////////////////////////////////////////
// Setting the Orientation maintains the same Twist.  This is done by
// compositing two rotations, "el" and "az", as the initial and final vectors
// for MoveCamera.  The third rotation ("twist") should remain invariant.
////////////////////////////////////////////////////////////////////////

void PigCAHV::setCameraOrientation(PigVector orientation, PigCoordSystem *cs)
{
    if (!_cahv_set) {
        printError("Attempt to use uninitialized CAHV camera model");
	return;
    }

    if (cs != _current_cs)
	orientation = _current_cs->convertVector(orientation, cs);

    PigVector y_axis(0.0, 1.0, 0.0);
    PigVector z_axis(0.0, 0.0, 1.0);

    // Composite rotation... el first, then az (quat multiplication is reversed)
    PigQuaternion rot1 = PigQuaternion(z_axis, _current_cs->getAz(_current_a)) *
			 PigQuaternion(y_axis, _current_cs->getEl(_current_a));
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

void PigCAHV::setCameraTwist(double twist)
{
    if (!_cahv_set) {
        printError("Attempt to use uninitialized CAHV camera model");
        return;
    }

    PigVector x_axis(1.0, 0.0, 0.0);
    PigVector y_axis(0.0, 1.0, 0.0);
    PigVector z_axis(0.0, 0.0, 1.0);

    // Complete composite is twist, el, az... which means az * el * twist.

    PigQuaternion azel = PigQuaternion(z_axis, _current_cs->getAz(_current_a)) *
			 PigQuaternion(y_axis, _current_cs->getEl(_current_a));

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
// This implementation should be sufficient for most uses, 
// subclasses should override it only if they need something
// unusual.
//
// The idea behind this implementation is to take the A vector
// and rotate it in the plane of H' and V' by the specified FOV.
// These vectors are then projected through the camera model to
// get the min/max values.
/////////////////////////////////////////////////////////////////
void PigCAHV::getMinMaxLS(double fov_h, double fov_v,
			  double *x_min, double *x_max,
			  double *y_min, double *y_max) const
{
    double c[3], a[3], h[3], v[3];
    double line, sample;
    PigVector axis, vector;
    PigPoint point;
    PigQuaternion *q;
    
    _current_c.getXYZ(c);
    _current_a.getXYZ(a);
    _current_h.getXYZ(h);
    _current_v.getXYZ(v);

    // compute H' and V'
    PigVector hPrime = _current_h - _current_a*(_current_a%_current_h);
    PigVector vPrime = _current_v - _current_a*(_current_a%_current_v);

    //Compute Sample max/min

    // axis of rotation
    axis = _current_a * hPrime;

    // quaternion that defines rotation
    q = new PigQuaternion(axis, PigDeg2Rad(fov_h/2));
    // get the vector
    vector = (*q) * _current_a;
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
    vector = (*q) * _current_a;
    delete q;
    // make sure it's a unit vector
    vector.normalize();
    // Add to C to get a point in XYZ space
    point = _current_c + vector;
    XYZtoLS(point, 0, &line, &sample, _current_cs);

    *x_min = sample;

    //Compute Line max/min

    // axis of rotation
    axis = _current_a * vPrime;

    // quaternion that defines rotation
    q = new PigQuaternion(axis, PigDeg2Rad(fov_v/2));
    // get the vector
    vector = (*q) * _current_a;
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
    vector = (*q) * _current_a;
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
// This method works for any of the CAHV subclasses, so the implementation
// is here.
//
// Note: this implementation assumes that all models are expressed in
// the same coordinate system!!!!
/////////////////////////////////////////////////////////////////

int PigCAHV::interpolateModels(int n_models, PigCameraModel *interp_models[],
                                double interp_values[], double value)
{
    cmod_t cmod_models[PIG_MAX_INTERP_CAMERA_MODELS];
    cmod_t out_cmod;

    if (n_models <= 0 || n_models > PIG_MAX_INTERP_CAMERA_MODELS)
	return 0;			// bad numbers

    for (int i=0; i < n_models; i++) {
	// Make sure it's a CAHV type
	if (strncasecmp(interp_models[i]->getModelName(), "CAHV", 4) != 0)
	    return 0;				// nope!
	((PigCAHV *)interp_models[i])->to_cmod_t(&cmod_models[i]);
    }

    int status = cmod_interp(n_models, cmod_models, interp_values,
		value, &out_cmod);

    if (status != 0) {
	printWarning("Interpolation failure for camera model");
	return 1;
    }

    // The type of this model should match the type of the interp models,
    // which should match the type of the output model.  So we can simply
    // call this->from_cmod_t() because the types match; an error will be
    // issued if they don't.

    status = from_cmod_t(&out_cmod);

    return status;
}

/////////////////////////////////////////////////////////////////
// Convert to the generic cmod_t type.
// Since we don't have a proper camera model size (xdim/ydim),
// we assume 1024.  That's right in most cases, and it doesn't really
// matter for the uses to which this is currently put (interpolation).
/////////////////////////////////////////////////////////////////

void PigCAHV::to_cmod_t(cmod_t *cmod)
{
    double c[3], a[3], h[3], v[3];

    _current_c.getXYZ(c);
    _current_a.getXYZ(a);
    _current_h.getXYZ(h);
    _current_v.getXYZ(v);

    cmod_init_cahv(1024, 1024, c, a, h, v, cmod);
}

/////////////////////////////////////////////////////////////////
// Convert from the generic cmod_t type.  Requires that "this"s type
// match the cmod_t type, or an error is printed and returned
// (success==1, error==0).
/////////////////////////////////////////////////////////////////

int PigCAHV::from_cmod_t(cmod_t *cmod)
{
    if (cmod->mclass != CMOD_CLASS_CAHV) {
	printWarning("Invalid model type in from_cmod_t()");
	return 0;
    }
    setInitialCAHV(cmod->u.cahv.c, cmod->u.cahv.a,
		   cmod->u.cahv.h, cmod->u.cahv.v,
		   _initial_cs);

    return 1;

}

