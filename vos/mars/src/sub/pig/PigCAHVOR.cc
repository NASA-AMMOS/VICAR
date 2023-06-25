/////////////////////////////////////////////////////////////////////////
// PigCAHVOR
//
// Implements the CAHVOR camera model.
////////////////////////////////////////////////////////////////////////

#include "PigCAHVOR.h"
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

PigCAHVOR::PigCAHVOR(const char *mission, const char *instrument,
		     const char *version, const char *subtype,
		     const char *construction, const char *calibration)
         : PigCAHV(mission, instrument, version, 
		   subtype, construction, calibration)
{
	// Empty
}

PigCAHVOR::PigCAHVOR()
	: PigCAHV()
{
	// Empty
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigCAHVOR::~PigCAHVOR()
{
    // nothing to do...
}

////////////////////////////////////////////////////////////////////////
// I/O routines
////////////////////////////////////////////////////////////////////////

int PigCAHVOR::readFromFile(const char *filename, PigCoordSystem *cs)
{
    double c[3], a[3], h[3], v[3], o[3], r[3];
    // The variables below are discarded after the read
    double s[18][18], hs, hc, vs, vc, theta, s_int[5][5];

    int status = cmod_cahvor_read(filename, c, a, h, v, o, r, s,
				&hs, &hc, &vs, &vc, &theta, s_int);

    if (status != CAHVOR_SUCCESS)
	return status;		// SUCCESS is 0 for CAHV

    setInitialCAHVOR(c, a, h, v, o, r, cs);		// CS not changd

    return 0;
}

int PigCAHVOR::writeToFile(const char *filename) const
{
    double c[3], a[3], h[3], v[3], o[3], r[3];
    int xdim, ydim;
    double s[18][18], hs, hc, vs, vc, theta, s_int[5][5];
    char *comment = "Written from PigCAHVOR";

    _current_c.getXYZ(c);
    _current_a.getXYZ(a);
    _current_h.getXYZ(h);
    _current_v.getXYZ(v);
    _current_o.getXYZ(o);
    _current_r.getXYZ(r);
    cmod_cahv_internal(c,a,h,v, NULL, &hs, &hc, &vs, &vc, &theta, NULL);
    memset(s, 0, sizeof(s));
    memset(s_int, 0, sizeof(s_int));
    xdim = 1024;                  //!!!! unknown by this class
    ydim = 1024;

    int status = cmod_cahvor_write(filename, comment,
				   c, a, h, v, o, r, s,
                                   hs, hc, vs, vc, theta, s_int);

    if (status != CAHVOR_SUCCESS)
        return status;          // SUCCESS is 0 for CAHV

    return 0;
}

////////////////////////////////////////////////////////////////////////
// The label routines expect a PigFileModel now.

int PigCAHVOR::readFromLabel(PigFileModel *file, int instance)
{
    const LblCameraModel_typ *CameraModel;
    PigCoordSystem *cs;
    PigCSReference *cs_ref;
    int status;

    CameraModel = file->getLblCameraModel(instance);
    if (CameraModel == NULL) {
	printError("Unable to read CAHVOR camera model from label!");
	return 1;
    }

    if (!CameraModel->ModelType.Valid) {
	printError("Missing ModelType in label");
	return 1;
    }
    if (strcasecmp(CameraModel->ModelType.Value, "CAHVOR") != 0) {
	printError("ModelType in label does not match subclass type of CAHVOR");
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
	   CameraModel->ModelComponent6[2].Valid )) {
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

    setInitialCAHVOR(c,a,h,v,o,r, cs);

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

// int PigCAHVOR::writeToLabel(int unit, int instance,
// 				const LblCameraModel_typ *aux) const

// Internal routine, used by subclasses to fill in basic CAHVOR info

void PigCAHVOR::setupWriteToLabel(LblCameraModel_typ &CM, PigCoordSystem *cs)
								const
{
    // Let PigCAHV do most of the work
    PigCAHV::setupWriteToLabel(CM, cs);

    // It's CAHVOR, not CAHV
    strcpy(CM.ModelType.Value, "CAHVOR");
    CM.ModelType.Valid = 1;

    // Fill in the fields for the two extra components

    strcpy(CM.ModelComponentId[4].Value, "O");
    CM.ModelComponentId[4].Valid = 1;
    strcpy(CM.ModelComponentName[4].Value, "OPTICAL");
    CM.ModelComponentName[4].Valid = 1;
    strcpy(CM.ModelComponentUnit[4].Value, "N/A");
    CM.ModelComponentUnit[4].Valid = 1;
    PigVector o = cs->convertVector(_current_o, _current_cs);
    CM.ModelComponent5[0].Value = o.getX();
    CM.ModelComponent5[1].Value = o.getY();
    CM.ModelComponent5[2].Value = o.getZ();
    CM.ModelComponent5[0].Valid = 1;
    CM.ModelComponent5[1].Valid = 1;
    CM.ModelComponent5[2].Valid = 1;

    strcpy(CM.ModelComponentId[5].Value, "R");
    CM.ModelComponentId[5].Valid = 1;
    strcpy(CM.ModelComponentName[5].Value, "RADIAL");
    CM.ModelComponentName[5].Valid = 1;
    strcpy(CM.ModelComponentUnit[5].Value, "N/A");
    CM.ModelComponentUnit[5].Valid = 1;
    // No coord system for R
    CM.ModelComponent6[0].Value = _current_r.getX();
    CM.ModelComponent6[1].Value = _current_r.getY();
    CM.ModelComponent6[2].Value = _current_r.getZ();
    CM.ModelComponent6[0].Valid = 1;
    CM.ModelComponent6[1].Valid = 1;
    CM.ModelComponent6[2].Valid = 1;
}

////////////////////////////////////////////////////////////////////////
// Read the CM from a string.  For CAHVOR, only the CAHVOR vectors themselves
// are read.  Any error will return 1.  The parser is not very forgiving.

int PigCAHVOR::readFromString(const char *string, PigCoordSystem *cs)
{
    if (cs == NULL)
	cs = _initial_cs;

    double c[3], a[3], h[3], v[3], o[3], r[3];

    int status = sscanf(string,
	"CAHVOR: C=(%lf,%lf,%lf) A=(%lf,%lf,%lf) H=(%lf,%lf,%lf) V=(%lf,%lf,%lf) O=(%lf,%lf,%lf) R=(%lf,%lf,%lf)",
		&c[0], &c[1], &c[2], &a[0], &a[1], &a[2],
		&h[0], &h[1], &h[2], &v[0], &v[1], &v[2],
		&o[0], &o[1], &o[2], &r[0], &r[1], &r[2]);
    if (status != 18)		// 12 items to read
	return 1;

    setInitialCAHVOR(c, a, h, v, o, r, cs);

    return 0;
}

////////////////////////////////////////////////////////////////////////
// Write the CM to a string.  For CAHVOR, only the CAHVOR vectors themselves
// are written.

int PigCAHVOR::writeToString(char *string, int max_length)
{
    char buf[350];

    sprintf(buf,
	"CAHVOR: C=(%lf,%lf,%lf) A=(%lf,%lf,%lf) H=(%lf,%lf,%lf) V=(%lf,%lf,%lf) O=(%lf,%lf,%lf) R=(%lf,%lf,%lf)",
		_current_c.getX(), _current_c.getY(), _current_c.getZ(),
		_current_a.getX(), _current_a.getY(), _current_a.getZ(),
		_current_h.getX(), _current_h.getY(), _current_h.getZ(),
		_current_v.getX(), _current_v.getY(), _current_v.getZ(),
		_current_o.getX(), _current_o.getY(), _current_o.getZ(),
		_current_r.getX(), _current_r.getY(), _current_r.getZ());
    if (strlen(buf) + 1 >= max_length)
	return 1;			// whoops, not big enough
    strcpy(string, buf);

    return 0;
}

////////////////////////////////////////////////////////////////////////
// Set the coordinate systems
////////////////////////////////////////////////////////////////////////

void PigCAHVOR::setInitialCoordSystem(PigCoordSystem *cs)
{
    if (_cahv_set && cs != _initial_cs) {	// change initial params
	_orig_c = cs->convertPoint(_orig_c, _initial_cs);
	_orig_a = cs->convertVector(_orig_a, _initial_cs);
	_orig_h = cs->convertVector(_orig_h, _initial_cs);
	_orig_v = cs->convertVector(_orig_v, _initial_cs);
	_orig_o = cs->convertVector(_orig_o, _initial_cs);
	// r is unchanged
    }
    _initial_cs = cs;
    setCoordSystem(cs);
}

void PigCAHVOR::setCoordSystem(PigCoordSystem *cs)
{
    PigCoordSystem *old_cs = _current_cs;
    PigCAHV::setCoordSystem(cs);
    _current_o = cs->convertVector(_current_o, old_cs);
    // r is unchanged
}

////////////////////////////////////////////////////////////////////////
// Set the initial CAHV vectors
////////////////////////////////////////////////////////////////////////

void PigCAHVOR::setInitialCAHV(const PigPoint c, const PigVector a,
				const PigVector h, const PigVector v,
				PigCoordSystem *cs)
{
    PigCAHV::setInitialCAHV(c, a, h, v, cs);
    _orig_o = _orig_a;
    _current_o = _current_a;
    _orig_r.setXYZ(0, 0, 0);
    _current_r =  _orig_r;
}

void PigCAHVOR::setInitialCAHV(const double c[3], const double a[3],
				const double h[3], const double v[3],
				PigCoordSystem *cs)
{
    PigCAHV::setInitialCAHV(c, a, h, v, cs);
    _orig_o = _orig_a;
    _current_o = _current_a;

    _orig_r.setXYZ(0, 0, 0);
    _current_r = _orig_r;
}

////////////////////////////////////////////////////////////////////////
// Set the initial CAHVOR vectors
////////////////////////////////////////////////////////////////////////

void PigCAHVOR::setInitialCAHVOR(const PigPoint c, const PigVector a,
				const PigVector h, const PigVector v,
				const PigVector o, const PigVector r,
				PigCoordSystem *cs)
{
    PigCAHV::setInitialCAHV(c, a, h, v, cs);
    _orig_o = _initial_cs->convertVector(o, cs);
    _orig_r = r;
    _current_o = _current_cs->convertVector(o, cs);
    _current_r = r;
}

void PigCAHVOR::setInitialCAHVOR(const double c[3], const double a[3],
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
// Set the current CAHV vectors
////////////////////////////////////////////////////////////////////////

void PigCAHVOR::setCurrentCAHV(const PigPoint c, const PigVector a,
				const PigVector h, const PigVector v,
				PigCoordSystem *cs)
{
    PigCAHV::setCurrentCAHV(c, a, h, v, cs);
    _current_o = _current_a;
    _current_r.setXYZ(0, 0, 0);
}

void PigCAHVOR::setCurrentCAHV(const double c[3], const double a[3],
				const double h[3], const double v[3],
				PigCoordSystem *cs)
{
    PigCAHV::setCurrentCAHV(c, a, h, v, cs);
    _current_o = _current_a;
    _current_r.setXYZ(0, 0, 0);
}

////////////////////////////////////////////////////////////////////////
// Set the current CAHVOR vectors
////////////////////////////////////////////////////////////////////////

void PigCAHVOR::setCurrentCAHVOR(const PigPoint c, const PigVector a,
				const PigVector h, const PigVector v,
				const PigVector o, const PigVector r,
				PigCoordSystem *cs)
{
    PigCAHV::setCurrentCAHV(c, a, h, v, cs);
    _current_o = _current_cs->convertVector(o, cs);
    _current_r = r;
}

void PigCAHVOR::setCurrentCAHVOR(const double c[3], const double a[3],
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
// Get the current CAHVOR vectors
////////////////////////////////////////////////////////////////////////

void PigCAHVOR::getCurrentCAHVOR(PigPoint &c, PigVector &a,
				PigVector &h, PigVector &v,
				PigVector &o, PigVector &r)
{
    PigCAHV::getCurrentCAHV(c, a, h, v);
    o = _current_o;
    r = _current_r;
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

void PigCAHVOR::moveCamera(const PigPoint initial_point,
			 const PigQuaternion initial_orientation,
			 const PigPoint final_point,
			 const PigQuaternion final_orientation,
			 PigCoordSystem *cs)
{
    double p_i[3], q_i[4];
    double p_f[3], q_f[4];
    double c_i[3], a_i[3], h_i[3], v_i[3], o_i[3], r_i[3];
    double c_f[3], a_f[3], h_f[3], v_f[3], o_f[3], r_f[3];
	

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

    cmod_cahvor_move(p_i, q_i, c_i, a_i, h_i, v_i, o_i, r_i,
		     p_f, q_f, c_f, a_f, h_f, v_f, o_f, r_f);

    _current_c.setXYZ(c_f);
    _current_a.setXYZ(a_f);
    _current_h.setXYZ(h_f);
    _current_v.setXYZ(v_f);
    _current_o.setXYZ(o_f);
    _current_r.setXYZ(r_f);

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

void PigCAHVOR::scaleCamera(const double x_factor, 
			    const double y_factor)
{
    double a_i[3], h_i[3], v_i[3];
    double h_f1[3], v_f1[3];
    double h_f2[3], v_f2[3];
    double h_f[3], v_f[3];
  
    _current_a.getXYZ(a_i);
    _current_h.getXYZ(h_i);
    _current_v.getXYZ(v_i);
 
    cmod_cahvor_shift(-0.5, -0.5, a_i, h_i, v_i, NULL, h_f1, v_f1, NULL);
    cmod_cahvor_scale(x_factor, y_factor, h_f1, v_f1, NULL, h_f2, v_f2, NULL);
    cmod_cahvor_shift(0.5, 0.5, a_i, h_f2, v_f2, NULL, h_f, v_f, NULL);

    _current_h.setXYZ(h_f);
    _current_v.setXYZ(v_f);
}

///////////////////////////////////////////////////////////////////////
// This function shifts a camera model. The shift values should be
// understood as the coordinates of the original model where the origin
// will fall in the new one.  In another words first_line, first_sample
// is the start of the image w.r.t. the Full Frame.
///////////////////////////////////////////////////////////////////////

void PigCAHVOR::shiftCamera(const double dx, 
			    const double dy)
{
    double a_i[3];
    double h_i[3], v_i[3];
    double h_f[3], v_f[3];

    _current_a.getXYZ(a_i);  
    _current_h.getXYZ(h_i);
    _current_v.getXYZ(v_i);
 
    cmod_cahvor_shift(dx, dy, a_i, h_i, v_i, NULL, h_f, v_f, NULL);

    _current_h.setXYZ(h_f);
    _current_v.setXYZ(v_f);
}

////////////////////////////////////////////////////////////////////////
// Resets the camera pointing to the "reference" location (usually
// where the calibration was taken), in preparation for a new pointing.
// This resets the coordinate system as well.
////////////////////////////////////////////////////////////////////////

void PigCAHVOR::resetCameraLocation()
{
    PigCAHV::resetCameraLocation();
    _current_o = _orig_o;
    _current_r = _orig_r;
}

////////////////////////////////////////////////////////////////////////
// Copy the Current parameters to the Initial ("reference") model
// that is used by resetCameraLocation().  This is often used to
// modify the initial model for downsample/subframe before pointing
// takes place.
////////////////////////////////////////////////////////////////////////

void PigCAHVOR::setInitialFromCurrent()
{
    PigCAHV::setInitialFromCurrent();
    _orig_o = _current_o;
    _orig_r = _current_r;
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

void PigCAHVOR::LStoLookVector(const double line, const double sample,
			PigPoint &origin, PigVector &look_direction,
			PigCoordSystem *cs) const
{
    double ls[2];
    double pos[3], uvec[3];
    double c[3], a[3], h[3], v[3], o[3], r[3];

    ls[0] = sample;
    ls[1] = line;

    _current_c.getXYZ(c);
    _current_a.getXYZ(a);
    _current_h.getXYZ(h);
    _current_v.getXYZ(v);
    _current_o.getXYZ(o);
    _current_r.getXYZ(r);

    //!!!! TBD: make use of "approx" flag?  (but not used in cahvor code!)
    cmod_cahvor_2d_to_3d(ls, c, a, h, v, o, r, FALSE, pos, uvec, NULL);

    origin.setXYZ(pos);
    look_direction.setXYZ(uvec);

    if (cs != _current_cs && cs != NULL) {
	origin = cs->convertPoint(origin, _current_cs);
	look_direction = cs->convertVector(look_direction, _current_cs);
    }
}



void PigCAHVOR::XYZtoLS(const PigPoint &xyz_in, const int infinity_flag,
			double *line, double *sample, PigCoordSystem *cs) const
{
   double range;
   XYZtoLS(xyz_in, infinity_flag, line, sample, cs, &range, NULL);   
}


void PigCAHVOR::XYZtoLS(const PigPoint &xyz_in, const int infinity_flag,
			double *line, double *sample, PigCoordSystem *cs,
                        double *range, double par[2][3]) const
{
    double ls[2];
    double pos[3];
    double c[3], a[3], h[3], v[3], o[3], r[3];
//    double range;

    // If infinity, assume XYZ point is the look vector, and project the
    // vanishing point.  This should use the cmod_cahv code, but a)
    // cmod_cahv_3d_to_2d_ray() calculates a bunch of stuff we don't need
    // (the back projection of the ray) and this is fairly time-crtitical
    // code, and b) there appears to be no cahvor form of this function.
    // Note that for the infinity case, the CS conversion needs to use
    // the vector form, not the point form!
    // The CAHVOR version of the infinity algorithm was supplied by Todd
    // Litwin; at the time of this writing is is not in the cmod library.

    if (infinity_flag) {
	PigVector look = xyz_in;
	if (cs != _current_cs && cs != NULL) {
	    look = _current_cs->convertVector(look, cs);
	}

	// Equivalent to part 1 of cmod_cahvor_3d_to_2d_ray().

	// Calculate (p' - c).  Note p' is never computed directly,
	// but is understood to be a unit distance from c in the
	// direction of the vanishing point.

	double omega = look % _current_o;
	PigVector lambda = look - (_current_o * omega);
	double tau = (lambda % lambda) / (omega * omega);
	double mu = _current_r.getX() + (_current_r.getY() * tau) +
			(_current_r.getZ() * tau * tau);
	PigVector pp_c = (lambda * mu) + look;

	// Calculate alpha, beta, gamma, which are (p' - c)
	// dotted with a, h, v, respectively

	double alpha = pp_c % _current_a;
	double beta = pp_c % _current_h;
	double gamma = pp_c % _current_v;

	// Calculate the projection

	*sample = beta / alpha;
	*line = gamma / alpha;

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


    //!!!! Make use of the "approx" flag!  (it *IS* used here)
    //cmod_cahvor_3d_to_2d(pos, c, a, h, v, o, r, FALSE, &range, ls, NULL);
    cmod_cahvor_3d_to_2d(pos, c, a, h, v, o, r, FALSE, range, ls, par);

    *sample = ls[0];
    *line = ls[1];
}

/////////////////////////////////////////////////////////////////
// Convert to the generic cmod_t type.
// Since we don't have a proper camera model size (xdim/ydim),
// we assume 1024.  That's right in most cases, and it doesn't really
// matter for the uses to which this is currently put (interpolation).
/////////////////////////////////////////////////////////////////

void PigCAHVOR::to_cmod_t(cmod_t *cmod)
{
    double c[3], a[3], h[3], v[3], o[3], r[3];

    _current_c.getXYZ(c);
    _current_a.getXYZ(a);
    _current_h.getXYZ(h);
    _current_v.getXYZ(v);
    _current_o.getXYZ(o);
    _current_r.getXYZ(r);

    cmod_init_cahvor(1024, 1024, c, a, h, v, o, r, cmod);
}

/////////////////////////////////////////////////////////////////
// Convert from the generic cmod_t type.  Requires that "this"s type
// match the cmod_t type, or an error is printed and returned
// (success==1, error==0).
/////////////////////////////////////////////////////////////////

int PigCAHVOR::from_cmod_t(cmod_t *cmod)
{
    if (cmod->mclass != CMOD_CLASS_CAHVOR) {
        printWarning("Invalid model type in from_cmod_t()");
        return 0;
    }
    setInitialCAHVOR(cmod->u.cahvor.c, cmod->u.cahvor.a, cmod->u.cahvor.h,
		     cmod->u.cahvor.v, cmod->u.cahvor.o, cmod->u.cahvor.r,
		     _initial_cs);

    return 1;

}


