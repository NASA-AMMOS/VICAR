/////////////////////////////////////////////////////////////////////////
// PigCAHVORE
//
// Implements the CAHVORE camera model.
////////////////////////////////////////////////////////////////////////

#include "PigCAHV.h"
#include "PigCAHVORE.h"
#include "PigCoordSystem.h"
#include "PigQuaternion.h"
#include "PigCSReference.h"
#include "PigFileModel.h"
#include "PigMission.h"
#include "return_status.h"
#include "cahvor.h"

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigCAHVORE::PigCAHVORE(const char *mission, const char *instrument,
			   const char *version, const char *subtype,
			   const char *construction, const char *calibration)
	       : PigCAHVOR(mission, instrument, version, 
					   subtype, construction, calibration)
{
  // Empty
}

PigCAHVORE::PigCAHVORE()
	: PigCAHVOR()
{
	// Empty
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigCAHVORE::~PigCAHVORE()
{
    // nothing to do...
}

////////////////////////////////////////////////////////////////////////
// I/O routines
////////////////////////////////////////////////////////////////////////

int PigCAHVORE::readFromFile(const char *filename, PigCoordSystem *cs)
{
    int xdim, ydim, mtype;
    double mparm, c[3], a[3], h[3], v[3], o[3], r[3], e[3];
    // The variables below are discarded after the read
    double s[21][21], hs, hc, vs, vc, theta, s_int[5][5];

    int status = cmod_cahvore_read(filename, &xdim, &ydim, &mtype, &mparm, 
				   c, a, h, v, o, r, e, s,
				   &hs, &hc, &vs, &vc, &theta, s_int);

    if (status != CAHVOR_SUCCESS)
	return status;		// SUCCESS is 0 for CAHVORE

    // CS not changed
    setInitialCAHVORE(c, a, h, v, o, r, e, mtype, mparm, cs);  

    return 0;
}

int PigCAHVORE::writeToFile(const char *filename) const
{
    double c[3], a[3], h[3], v[3], o[3], r[3], e[3];
    int mtype;
    double mparm;
    int xdim, ydim;
    double s[21][21], hs, hc, vs, vc, theta, s_int[5][5];
    char *comment = "Written from PigCAHVORE";

    _current_c.getXYZ(c);
    _current_a.getXYZ(a);
    _current_h.getXYZ(h);
    _current_v.getXYZ(v);
    _current_o.getXYZ(o);
    _current_r.getXYZ(r);
    _current_e.getXYZ(e);
    mtype = _mtype;
    mparm = _mparm;
    cmod_cahv_internal(c,a,h,v, NULL, &hs, &hc, &vs, &vc, &theta, NULL);
    memset(s, 0, sizeof(s));
    memset(s_int, 0, sizeof(s_int));
    // Having a negative value is treated by cmod routine as 
    // there is no value
    xdim = -1;
    ydim = -1;

    int status = cmod_cahvore_write(filename, comment, xdim, ydim,
                                mtype, mparm, c, a, h, v, o, r, e, s,
                                hs, hc, vs, vc, theta, s_int);

    if (status != CAHVOR_SUCCESS)
        return status;          // SUCCESS is 0 for CAHV

    return 0;
}

////////////////////////////////////////////////////////////////////////
// The label routines expect a PigFileModel now.

int PigCAHVORE::readFromLabel(PigFileModel *file, int instance)
{
    const LblCameraModel_typ *CameraModel;
    PigCoordSystem *cs;
    PigCSReference *cs_ref;
    int status;

    CameraModel = file->getLblCameraModel(instance);
    if (CameraModel == NULL) {
        printError("Unable to read CAHVORE camera model from label!");
        return 1;
    }

    if (!CameraModel->ModelType.Valid) {
	printError("Missing ModelType in label");
	return 1;
    }
    if (strcasecmp(CameraModel->ModelType.Value, "CAHVORE") != 0) {
	printError("ModelType in label does not match subclass type of CAHVORE");
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

        cs = _initial_cs;                       // just in case...
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
	   CameraModel->ModelComponent4[2].Valid &&


	   CameraModel->ModelComponent5[0].Valid &&         //O
	   CameraModel->ModelComponent5[1].Valid &&
	   CameraModel->ModelComponent5[2].Valid &&


	   CameraModel->ModelComponent6[0].Valid &&         //R
	   CameraModel->ModelComponent6[1].Valid &&
	   CameraModel->ModelComponent6[2].Valid &&


	   CameraModel->ModelComponent7[0].Valid &&         //E
	   CameraModel->ModelComponent7[1].Valid &&
	   CameraModel->ModelComponent7[2].Valid &&

	   CameraModel->ModelComponent8[0].Valid &&        //TYPE

	   CameraModel->ModelComponent9[0].Valid )) {      //PARM
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
    PigVector o(CameraModel->ModelComponent5[0].Value,
		CameraModel->ModelComponent5[1].Value,
		CameraModel->ModelComponent5[2].Value);
    PigVector r(CameraModel->ModelComponent6[0].Value,
		CameraModel->ModelComponent6[1].Value,
		CameraModel->ModelComponent6[2].Value);
    PigVector e(CameraModel->ModelComponent7[0].Value,
		CameraModel->ModelComponent7[1].Value,
		CameraModel->ModelComponent7[2].Value);
    _mtype = (int)CameraModel->ModelComponent8[0].Value;
    _mparm = CameraModel->ModelComponent9[0].Value;	

    setInitialCAHVORE(c,a,h,v,o,r,e, _mtype, _mparm, cs);

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
// structure if desired:  CalibrationSourceId, CameraModelDesc,
// CameraModelName, GeometrySourceId.  Set the Valid flag for any fields
// set.  Defaults are provided for required fields.  aux may be passed
// in as NULL.

// NOTE:  This function is entirely implemented in the PigCAHV superclass.
// The only difference is the virtual function setupWriteToLabel().

// int PigCAHVORE::writeToLabel(int unit, int instance,
// 				const LblCameraModel_typ *aux) const

// Internal routine, used by subclasses to fill in basic CAHVORE info

void PigCAHVORE::setupWriteToLabel(LblCameraModel_typ &CM, PigCoordSystem *cs)
								const
{
    // Let PigCAHVOR do most of the work
    PigCAHVOR::setupWriteToLabel(CM, cs);

    // It's CAHVORE, not CAHVOR
    strcpy(CM.ModelType.Value, "CAHVORE");
    CM.ModelType.Valid = 1;

    // Fill in the fields for the extra components

    strcpy(CM.ModelComponentId[6].Value, "E");
    CM.ModelComponentId[6].Valid = 1;
    strcpy(CM.ModelComponentName[6].Value, "ENTRANCE");
    CM.ModelComponentName[6].Valid = 1;
    strcpy(CM.ModelComponentUnit[6].Value, "N/A");
    CM.ModelComponentUnit[6].Valid = 1;
    // No coord system for E
    CM.ModelComponent7[0].Value = _current_e.getX();
    CM.ModelComponent7[1].Value = _current_e.getY();
    CM.ModelComponent7[2].Value = _current_e.getZ();
    CM.ModelComponent7[0].Valid = 1;
    CM.ModelComponent7[1].Valid = 1;
    CM.ModelComponent7[2].Valid = 1;

    //setup mtype, mparm
    CM.ModelComponent8[0].Value = _mtype;
    CM.ModelComponent9[0].Value = _mparm;
    CM.ModelComponent8[0].Valid = 1;
    CM.ModelComponent9[0].Valid = 1;


}

////////////////////////////////////////////////////////////////////////
// Read the CM from a string.  For CAHVORE, only the CAHVORE vectors themselves
// are read.  Any error will return 1.  The parser is not very forgiving.

int PigCAHVORE::readFromString(const char *string, PigCoordSystem *cs)
{
    if (cs == NULL)
	cs = _initial_cs;

    double c[3], a[3], h[3], v[3], o[3], r[3], e[3];
    int mtype;
    double mparm;

    int status = sscanf(string,
	"CAHVORE: C=(%lf,%lf,%lf) A=(%lf,%lf,%lf) H=(%lf,%lf,%lf) V=(%lf,%lf,%lf) O=(%lf,%lf,%lf) R=(%lf,%lf,%lf) E=(%lf,%lf,%lf) mtype = %ld mparm = %lf",
		&c[0], &c[1], &c[2], &a[0], &a[1], &a[2],
		&h[0], &h[1], &h[2], &v[0], &v[1], &v[2],
		&o[0], &o[1], &o[2], &r[0], &r[1], &r[2],
		&e[0], &e[1], &e[2],
		&mtype, &mparm);
    if (status != 23)		// 15 items to read
	return 1;

    setInitialCAHVORE(c, a, h, v, o, r, e, mtype, mparm, cs);

    return 0;
}

////////////////////////////////////////////////////////////////////////
// Write the CM to a string.  For CAHVORE, only the CAHVORE vectors themselves
// are written.

int PigCAHVORE::writeToString(char *string, int max_length)
{
    char buf[350];

    sprintf(buf,
	"CAHVORE: C=(%lf,%lf,%lf) A=(%lf,%lf,%lf) H=(%lf,%lf,%lf) V=(%lf,%lf,%lf) O=(%lf,%lf,%lf) R=(%lf,%lf,%lf) E=(%lf,%lf,%lf) mtype = %ld mparm = %lf",
	    _current_c.getX(), _current_c.getY(), _current_c.getZ(),
	    _current_a.getX(), _current_a.getY(), _current_a.getZ(),
	    _current_h.getX(), _current_h.getY(), _current_h.getZ(),
	    _current_v.getX(), _current_v.getY(), _current_v.getZ(),
	    _current_o.getX(), _current_o.getY(), _current_o.getZ(),
	    _current_r.getX(), _current_r.getY(), _current_r.getZ(),
	    _current_e.getX(), _current_e.getY(), _current_e.getZ(),
	    _mtype, _mparm);
    if (strlen(buf) + 1 >= max_length)
	return 1;			// whoops, not big enough
    strcpy(string, buf);

    return 0;
}

////////////////////////////////////////////////////////////////////////
// Set the coordinate systems
////////////////////////////////////////////////////////////////////////

void PigCAHVORE::setInitialCoordSystem(PigCoordSystem *cs)
{
    if (_cahv_set && cs != _initial_cs) {	// change initial params
	_orig_c = cs->convertPoint(_orig_c, _initial_cs);
	_orig_a = cs->convertVector(_orig_a, _initial_cs);
	_orig_h = cs->convertVector(_orig_h, _initial_cs);
	_orig_v = cs->convertVector(_orig_v, _initial_cs);
	_orig_o = cs->convertVector(_orig_o, _initial_cs);
	// r is unchanged
	// e is unchanged
    }
    _initial_cs = cs;
    setCoordSystem(cs);
}

void PigCAHVORE::setCoordSystem(PigCoordSystem *cs)
{
    PigCoordSystem *old_cs = _current_cs;
    PigCAHV::setCoordSystem(cs);
    _current_o = cs->convertVector(_current_o, old_cs);
    // r is unchanged
    // e is unchanged
}

////////////////////////////////////////////////////////////////////////
// Set the initial CAHV vectors
////////////////////////////////////////////////////////////////////////

void PigCAHVORE::setInitialCAHV(const PigPoint c, const PigVector a,
				const PigVector h, const PigVector v,
				PigCoordSystem *cs)
{
    _mtype = 1;
    _mparm = 1;
    PigCAHVOR::setInitialCAHV(c, a, h, v, cs);
    _orig_e.setXYZ(0, 0, 0);
    _current_e = _orig_e;
}

void PigCAHVORE::setInitialCAHV(const double c[3], const double a[3],
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
// Set the initial CAHVOR vectors
////////////////////////////////////////////////////////////////////////

void PigCAHVORE::setInitialCAHVOR(const PigPoint c, const PigVector a,
				  const PigVector h, const PigVector v,
				  const PigVector o, const PigVector r,
				  PigCoordSystem *cs)
{
    _mtype = 1;
    _mparm = 1;
    PigCAHVOR::setInitialCAHVOR(c, a, h, v, o, r, cs);
    _orig_e.setXYZ(0, 0, 0);
    _current_e = _orig_e;
}

void PigCAHVORE::setInitialCAHVOR(const double c[3], const double a[3],
				  const double h[3], const double v[3],
				  const double o[3], const double r[3],
				  PigCoordSystem *cs)
{
    PigPoint cc(c);
    PigVector aa(a);
    PigVector hh(h);
    PigVector vv(v);
    PigVector oo(o);
    PigVector rr(r);
    setInitialCAHVOR(cc, aa, hh, vv, oo, rr, cs);
}
////////////////////////////////////////////////////////////////////////
// Set the initial CAHVORE vectors
////////////////////////////////////////////////////////////////////////

void PigCAHVORE::setInitialCAHVORE(const PigPoint c, const PigVector a,
				   const PigVector h, const PigVector v,
				   const PigVector o, const PigVector r,
				   const PigVector e, 
				   int mtype, double mparm,
				   PigCoordSystem *cs)
{
  PigCAHVOR::setInitialCAHVOR(c, a, h, v, o, r, cs);
  _mtype = mtype;
  _mparm = mparm;
  _orig_e = e;
  _current_e = e;
}

void PigCAHVORE::setInitialCAHVORE(const double c[3], const double a[3],
				   const double h[3], const double v[3],
				   const double o[3], const double r[3],
				   const double e[3], 
				   int mtype, double mparm,
				   PigCoordSystem *cs)
{
    PigPoint  cc(c);
    PigVector aa(a);
    PigVector hh(h);
    PigVector vv(v);
    PigVector oo(o);
    PigVector rr(r);
    PigVector ee(e);
    setInitialCAHVORE(cc, aa, hh, vv, oo, rr, ee, mtype, mparm, cs);
}

////////////////////////////////////////////////////////////////////////
// Set the current CAHV vectors
////////////////////////////////////////////////////////////////////////

void PigCAHVORE::setCurrentCAHV(const PigPoint c, const PigVector a,
				const PigVector h, const PigVector v,
				PigCoordSystem *cs)
{
    PigCAHVOR::setCurrentCAHV(c, a, h, v, cs);
    _current_e.setXYZ(0, 0, 0);
}

void PigCAHVORE::setCurrentCAHV(const double c[3], const double a[3],
				const double h[3], const double v[3],
				PigCoordSystem *cs)
{
    PigCAHVOR::setCurrentCAHV(c, a, h, v, cs);
    _current_e.setXYZ(0, 0, 0);
}
////////////////////////////////////////////////////////////////////////
// Set the current CAHVOR vectors
////////////////////////////////////////////////////////////////////////

void PigCAHVORE::setCurrentCAHVOR(const PigPoint c, const PigVector a,
				  const PigVector h, const PigVector v,
				  const PigVector o, const PigVector r,
				  PigCoordSystem *cs)
{
    PigCAHVOR::setCurrentCAHVOR(c, a, h, v, o, r, cs);
    _current_e.setXYZ(0, 0, 0);
}

void PigCAHVORE::setCurrentCAHVOR(const double c[3], const double a[3],
				const double h[3], const double v[3],
				const double o[3], const double r[3],
				PigCoordSystem *cs)
{
    PigPoint cc(c);
    PigVector aa(a);
    PigVector hh(h);
    PigVector vv(v);
    PigVector oo(o);
    PigVector rr(r);
    setCurrentCAHVOR(cc, aa, hh, vv, oo, rr, cs);
}
////////////////////////////////////////////////////////////////////////
// Set the current CAHVORE vectors
////////////////////////////////////////////////////////////////////////

void PigCAHVORE::setCurrentCAHVORE(const PigPoint c, const PigVector a,
				   const PigVector h, const PigVector v,
				   const PigVector o, const PigVector r,
				   const PigVector e,
				   int mtype, double mparm, 
				   PigCoordSystem *cs)
{
    PigCAHVOR::setCurrentCAHVOR(c, a, h, v, o, r, cs);
    _mtype = mtype;
    _mparm = mparm;
    _current_e = e;
}

void PigCAHVORE::setCurrentCAHVORE(const double c[3], const double a[3],
				   const double h[3], const double v[3],
				   const double o[3], const double r[3],
				   const double e[3], 
				   int mtype, double mparm,
				   PigCoordSystem *cs)
{
    PigPoint cc(c);
    PigVector aa(a);
    PigVector hh(h);
    PigVector vv(v);
    PigVector oo(o);
    PigVector rr(r);
    PigVector ee(e);
    setCurrentCAHVORE(cc, aa, hh, vv, oo, rr, ee, mtype, mparm, cs);
}

////////////////////////////////////////////////////////////////////////
// Get the current CAHVORE vectors
////////////////////////////////////////////////////////////////////////

void PigCAHVORE::getCurrentCAHVORE(PigPoint &c, PigVector &a,
				   PigVector &h, PigVector &v,
				   PigVector &o, PigVector &r,
				   PigVector &e,
				   int &mtype, double &mparm)
{
    PigCAHVOR::getCurrentCAHVOR(c, a, h, v, o, r);
    mtype = _mtype;
    mparm = _mparm;
    e = _current_e;
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

void PigCAHVORE::moveCamera(const PigPoint initial_point,
			    const PigQuaternion initial_orientation,
			    const PigPoint final_point,
			    const PigQuaternion final_orientation,
			    PigCoordSystem *cs)
{
    double p_i[3], q_i[4];
    double p_f[3], q_f[4];
    double c_i[3], a_i[3], h_i[3], v_i[3], o_i[3], r_i[3], e_i[3];
    double c_f[3], a_f[3], h_f[3], v_f[3], o_f[3], r_f[3], e_f[3];

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
    _current_o.getXYZ(o_i);
    _current_r.getXYZ(r_i);
    _current_e.getXYZ(e_i);

    cmod_cahvore_move(p_i, q_i, c_i, a_i, h_i, v_i, o_i, r_i, e_i,
		      p_f, q_f, c_f, a_f, h_f, v_f, o_f, r_f, e_f);

    _current_c.setXYZ(c_f);
    _current_a.setXYZ(a_f);
    _current_h.setXYZ(h_f);
    _current_v.setXYZ(v_f);
    _current_o.setXYZ(o_f);
    _current_r.setXYZ(r_f);
    _current_e.setXYZ(e_f);

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

void PigCAHVORE::scaleCamera(const double x_factor, 
			    const double y_factor)
{
    double a_i[3], h_i[3], v_i[3];
    double h_f1[3], v_f1[3];
    double h_f2[3], v_f2[3];
    double h_f[3], v_f[3];
  
    _current_a.getXYZ(a_i);
    _current_h.getXYZ(h_i);
    _current_v.getXYZ(v_i);
 
    cmod_cahvore_shift(-0.5, -0.5, a_i, h_i, v_i, NULL, h_f1, v_f1, NULL);
    cmod_cahvore_scale(x_factor, y_factor, h_f1, v_f1, NULL, h_f2, v_f2, NULL);
    cmod_cahvore_shift(0.5, 0.5, a_i, h_f2, v_f2, NULL, h_f, v_f, NULL);

    _current_h.setXYZ(h_f);
    _current_v.setXYZ(v_f);
}

///////////////////////////////////////////////////////////////////////
// This function shifts a camera model. The shift values should be
// understood as the coordinates of the original model where the origin
// will fall in the new one.  In another words first_line, first_sample
// is the start of the image w.r.t. the Full Frame.
///////////////////////////////////////////////////////////////////////

void PigCAHVORE::shiftCamera(const double dx, 
			    const double dy)
{
    double a_i[3];
    double h_i[3], v_i[3];
    double h_f[3], v_f[3];

    _current_a.getXYZ(a_i);  
    _current_h.getXYZ(h_i);
    _current_v.getXYZ(v_i);
 
    cmod_cahvore_shift(dx, dy, a_i, h_i, v_i, NULL, h_f, v_f, NULL);

    _current_h.setXYZ(h_f);
    _current_v.setXYZ(v_f);
}
////////////////////////////////////////////////////////////////////////
// Resets the camera pointing to the "reference" location (usually
// where the calibration was taken), in preparation for a new pointing.
// This resets the coordinate system as well.
////////////////////////////////////////////////////////////////////////

void PigCAHVORE::resetCameraLocation()
{
    PigCAHVOR::resetCameraLocation();
    _current_e = _orig_e;
}

////////////////////////////////////////////////////////////////////////
// Copy the Current paraemters to the Initial ("reference") model
// that is used by resetCameraLocation().  This is often used to
// modify the initial model for downsample/subframe before pointing
// takes place.
////////////////////////////////////////////////////////////////////////

void PigCAHVORE::setInitialFromCurrent()
{
    PigCAHVOR::setInitialFromCurrent();
    _orig_e = _current_e;
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

void PigCAHVORE::LStoLookVector(const double line, const double sample,
			PigPoint &origin, PigVector &look_direction,
			PigCoordSystem *cs) const
{
    double ls[2];
    double pos[3], uvec[3];
    double c[3], a[3], h[3], v[3], o[3], r[3], e[3];

    ls[0] = sample;
    ls[1] = line;

    _current_c.getXYZ(c);
    _current_a.getXYZ(a);
    _current_h.getXYZ(h);
    _current_v.getXYZ(v);
    _current_o.getXYZ(o);
    _current_r.getXYZ(r);
    _current_e.getXYZ(e);

    cmod_cahvore_2d_to_3d(ls, _mtype, _mparm,
			  c, a, h, v, o, r, e, 
			  FALSE, pos, uvec, NULL, NULL);

    origin.setXYZ(pos);
    look_direction.setXYZ(uvec);

    if (cs != _current_cs && cs != NULL) {
	origin = cs->convertPoint(origin, _current_cs);
	look_direction = cs->convertVector(look_direction, _current_cs);
    }
}


void PigCAHVORE::XYZtoLS(const PigPoint &xyz_in, const int infinity_flag,
			double *line, double *sample, PigCoordSystem *cs) const
{
   double range;
   XYZtoLS(xyz_in, infinity_flag, line, sample, cs, &range, NULL);   
}

void PigCAHVORE::XYZtoLS(const PigPoint &xyz_in, const int infinity_flag,
			double *line, double *sample, PigCoordSystem *cs,
                        double *range, double par[2][3]) const
{
    double ls[2];
    double pos[3], pos3[3], uvec3[3];
    double c[3], a[3], h[3], v[3], o[3], r[3], e[3];
    //double range;

    // If infinity, assume XYZ point is the look vector, and project the
    // vanishing point.
    // Note that for the infinity case, the CS conversion needs to use
    // the vector form, not the point form!

    if (infinity_flag) {
	PigVector look = xyz_in;
	if (cs != _current_cs && cs != NULL) {
	    look = _current_cs->convertVector(look, cs);
	}

	pos3[0] = xyz_in.getX();
	pos3[1] = xyz_in.getY();
	pos3[2] = xyz_in.getZ();

	uvec3[0] = look.getX();
	uvec3[1] = look.getY();
	uvec3[2] = look.getZ();
	

    _current_c.getXYZ(c);
    _current_a.getXYZ(a);
    _current_h.getXYZ(h);
    _current_v.getXYZ(v);
    _current_o.getXYZ(o);
    _current_r.getXYZ(r);
    _current_e.getXYZ(e);
 
	cmod_cahvore_3d_to_2d_point(_mtype, _mparm, 
				    c, a, h, v, o, r, e,
				    FALSE, pos3, uvec3, ls,
				    (double (*)[3]) NULL);

	*sample = ls[0];
	*line = ls[1];
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
    _current_o.getXYZ(o);
    _current_r.getXYZ(r);
    _current_e.getXYZ(e);

    //!!!! Make use of the "approx" flag!  (it *IS* used here)
    cmod_cahvore_3d_to_2d(pos, _mtype, _mparm,
			  c, a, h, v, o, r, e, 
			  FALSE, range, ls, par);

    *sample = ls[0];
    *line = ls[1];
}

/////////////////////////////////////////////////////////////////
// Convert to the generic cmod_t type.
// Since we don't have a proper camera model size (xdim/ydim),
// we assume 1024.  That's right in most cases, and it doesn't really
// matter for the uses to which this is currently put (interpolation).
/////////////////////////////////////////////////////////////////

void PigCAHVORE::to_cmod_t(cmod_t *cmod)
{
    double c[3], a[3], h[3], v[3], o[3], r[3], e[3];
    double mparm;
    int mtype;

    _current_c.getXYZ(c);
    _current_a.getXYZ(a);
    _current_h.getXYZ(h);
    _current_v.getXYZ(v);
    _current_o.getXYZ(o);
    _current_r.getXYZ(r);
    _current_e.getXYZ(e);
    mtype = _mtype;
    mparm = _mparm;

    cmod_init_cahvore(1024, 1024, mtype, mparm, c, a, h, v, o, r, e, cmod);
}


/////////////////////////////////////////////////////////////////
// Convert from the generic cmod_t type.  Requires that "this"s type
// match the cmod_t type, or an error is printed and returned
// (success==1, error==0).
/////////////////////////////////////////////////////////////////

int PigCAHVORE::from_cmod_t(cmod_t *cmod)
{
    if (cmod->mclass != CMOD_CLASS_CAHVORE) {
        printWarning("Invalid model type in from_cmod_t()");
        return 0;
    }
    setInitialCAHVORE(cmod->u.cahvore.c, cmod->u.cahvore.a, cmod->u.cahvore.h,
		      cmod->u.cahvore.v, cmod->u.cahvore.o, cmod->u.cahvore.r,
		      cmod->u.cahvore.e,
		      cmod->u.cahvore.mtype, cmod->u.cahvore.mparm,
                      _initial_cs);

    return 1;
}


