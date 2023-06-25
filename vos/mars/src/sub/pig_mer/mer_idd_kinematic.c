/***************************************************************************
 * File Name: idd_kinematic.c
 * 
 * This originally came from the MER FSW.  However, only one routine was
 * actually needed, with a few support routines, so it has been extracted
 * below and the rest of the FSW stripped out.  It does expect parameters
 * (things like arm segment lengths and joint locations) to be loaded into
 * the input arrays.
 *
 * Some names have been prefixed with "mer_" to avoid collisions with the
 * actual FSW routines, which are still needed in marsreach.
 *
 * Also included is a very stripped down version of the Matrix package, with
 * just the functions we need.
 *
 * rgd 2023-04-05, after original from Bob Bonitz (or D. Smyth for Matrix).
 ***************************************************************************/

#include <math.h>
#include <float.h>
#include <string.h>

#include "mer_idd_kinematic.h"

/************************************************************************/
/* Things from Matrix package						*/
/************************************************************************/

#define SIGN(a)  (((a)<0) ?  -1  :  1)

typedef struct _merVector {
    double v[3];
} merVector;

typedef struct _merMatrix {
    double m[3][3];
} merMatrix;

/* Quaternion is defined in the .h file */

/* Assign values to a Matrix
**===========================**
 * The values are specified in row order.  This makes it
 * easy to read, if you want.  Like this:
 *      Matrix m = Matrix_Assign( 0.0, 0.1, 0.2,
 *                                1.0, 1.1, 1.2,
 *                                2.0, 2.1, 2.2 );
 */
merMatrix merMatrix_Assign( double r0c0, double r0c1, double r0c2,
                      double r1c0, double r1c1, double r1c2,
                      double r2c0, double r2c1, double r2c2 )
{
    merMatrix m;

    m.m[0][0] = r0c0; m.m[0][1] = r0c1; m.m[0][2] = r0c2;
    m.m[1][0] = r1c0; m.m[1][1] = r1c1; m.m[1][2] = r1c2;
    m.m[2][0] = r2c0; m.m[2][1] = r2c1; m.m[2][2] = r2c2;

    return m;
}


/* Assign values to a Vector
**===========================**
*/
merVector merVector_Assign( double x, double y, double z )
{
    merVector v;

    v.v[0] = x; v.v[1] = y; v.v[2] = z;

    return v;
}


/* Assign values to a Quaternion
**===============================**
*/
merQuaternion merQuaternion_Assign( double a, double b, double c, double d )
{
    merQuaternion q;

    q.q[0] = a; q.q[1] = b; q.q[2] = c; q.q[3] = d;

    return q;
}


/* Vector Arithmetic
**===================**
*/
merVector merVector_VectorPlusVector( merVector a, merVector b )
{
    return merVector_Assign( a.v[0]+b.v[0], a.v[1]+b.v[1], a.v[2]+b.v[2] );
}


/*  Multiply Two Matrices, Result into a Third Matrix
**===================================================**
 *      result =: left . right
 */
merMatrix merMatrix_MatrixDotMatrix( merMatrix left, merMatrix right )
{
    merMatrix result;
    int    i, j, k;

    for ( i = 0  ;  i < 3  ;  ++i )
    {
        for ( j = 0  ;  j < 3  ;  ++j )
        {
            result.m[i][j] = 0.0;

            for ( k = 0  ;  k < 3  ;  ++k )
            {
                result.m[i][j] += left.m[i][k] * right.m[k][j];
            }
        }
    }
    return result;
}



/* Matrix-Vector Product, as for Coordinate Transformation
**=========================================================**
 *      result =: matrix . vector
 */
merVector merVector_MatrixDotVector( merMatrix m, merVector orig )
{
    merVector result;
    int    i, j;

    for ( i = 0  ;  i < 3  ;  ++i )
    {
        result.v[i] = 0.0;

        for ( j = 0  ;  j < 3  ;  ++j )
        {
            result.v[i] += m.m[i][j] * orig.v[j];
        }
    }
    return result;
}

/***************************************************************************
 * Name: idd_rot_to_quat
 *
 * Purpose: Convert a rotation matrix to a unit quaternion
 *
 *   Ref.: J. Funda, R.H. Taylor, and R.P. Paul, "On Homogeneous Transforms, 
 *   Quaternions, and Computational Efficiency", IEEE Transactions on 
 *   Robotics, and Automation, Vol. 6, No. 3, June 1990, pp. 382-388
 *
 *   This function uses v1, v2, and v3, respectivley as designators for 
 *   v', v'', and v''' used in the reference.
 *
 * Inputs: 
 *   Matrix rot - rotation matrix.
 *
 * Outputs: 
 * 
 * Returns: 
 *  Quaternion - computed unit quaternion
 *
 * Externally Read:
 *
 * Externally Modified:
 *
 * Limitations: Used by DIMES also.
 *
 ***************************************************************************/
merQuaternion meridd_rot_to_quat(
    merMatrix      R) {    /* rotation matrix                              */

    F64     nx, ny, nz, sx, sy, sz, ax, ay, az; 
    F64     s, x1, y1, z1, x2, y2, z2, x3, y3, z3, v[3], normalize, tmp;    
    int     largest;


    /* Extract parameters */
    nx = R.m[0][0];
    ny = R.m[1][0];
    nz = R.m[2][0];
    sx = R.m[0][1];
    sy = R.m[1][1];
    sz = R.m[2][1];
    ax = R.m[0][2];
    ay = R.m[1][2];
    az = R.m[2][2];

    /* Compute the scaler part of the quaternion */
    s = 0.5*sqrt(fabs(nx + sy + az + 1));

    /* Compute elements of v1 = [x1 y1 z1] */
    x1 = sz - ay;
    y1 = ax - nz;
    z1 = ny - sx;

    /* Test for the largest component of the rotation axis */
    if (sy > nx) {
        if (az > sy)
            largest = 3;
        else
            largest = 2;
    }
    else 
        largest = 1;

    /* Compute elements of v3 = [x3 y3 z3] */
    if (largest == 1) {
        x2 = nx - sy - az + 1;
        y2 = ny + sx;
        z2 = nz + ax;
        x3 = x1 + SIGN(x1)*x2;
        y3 = y1 + SIGN(x1)*y2;
        z3 = z1 + SIGN(x1)*z2; 
    }
    else if (largest == 2) {
        x2 = ny + sx;
        y2 = -nx + sy - az + 1;
        z2 = sz + ay;
        x3 = x1 + SIGN(y1)*x2;
        y3 = y1 + SIGN(y1)*y2;
        z3 = z1 + SIGN(y1)*z2;
    }
    else {
        x2 = nz + ax;
        y2 = sz + ay;
        z2 = -nx - sy + az + 1;
        x3 = x1 + SIGN(z1)*x2;
        y3 = y1 + SIGN(z1)*y2;
        z3 = z1 + SIGN(z1)*z2;
    }

    /*  Compute the vector elements of the quaternion */
    tmp = x3*x3 + y3*y3 + z3*z3;
    /* If scaler part close to 1, make it 1 */ 
    if ((s > (1.0 - 10*DBL_EPSILON)) || (tmp < 10*DBL_EPSILON)) {
        s = 1.0;
        v[0] = v[1] = v[2] = 0.0;
    }
    else {
        normalize = sqrt((1-s*s) / tmp);
        v[0] = normalize*x3;
        v[1] = normalize*y3;
        v[2] = normalize*z3;
    }
    
    return merQuaternion_Assign(v[0], v[1], v[2], s);
}

/***************************************************************************
 * Name: idd_quat_to_rot
 *
 * Purpose: Convert a quaternion to a rotation matrix.
 *
 *   Ref.: J. Funda, R.H. Taylor, and R.P. Paul, "On Homogeneous Transforms, 
 *   Quaternions, and Computational Efficiency", IEEE Transactions on 
 *   Robotics, and Automation, Vol. 6, No. 3, June 1990, pp. 382-388
 *
 * Inputs: 
 *  Quaternion q - computed unit quaternion
 *
 * Outputs: 
 * 
 * Returns: 
 *  Matrix - rotation matrix.
 *
 * Externally Read:
 *
 * Externally Modified:
 *
 * Limitations: 
 *
 ***************************************************************************/
merMatrix meridd_quat_to_rot(
    merQuaternion  q) {

    F64     x, y, z, q0;

    x = q.q[0];
    y = q.q[1];
    z = q.q[2];
    q0= q.q[3];

    return merMatrix_Assign( 1-2*(y*y+z*z),    2*(x*y-q0*z),   2*(x*z+q0*y),
                          2*(x*y+q0*z),     1-2*(x*x+z*z),  2*(y*z-q0*x),
                          2*(x*z-q0*y),     2*(y*z+q0*x),   1-2*(x*x+y*y) );
}    



/***************************************************************************
 * Name: mer_idd_fwd_kin
 *
 * Purpose: Compute the forward kinematics 
 *
 * Inputs: 
 *  const IddJoint      q       - joint angles in radians
 *  const IddPrmsDh*    dh_p    - pointer to DH parameters
 *  const IddPrmsFrame* frm_p   - pointer to rover-to-IDD transform
 *  const IddPrmsTool*  tool_p  - pointer to tool parameters
 *  IddTool             tool    - current tool
 *
 * Outputs: 
 *  IddPoseP            pose_p     - pointer to computed pose
 *  Quaternion *        rvrQtool_p - pointer to computed quaternion
 *
 * Returns: None.
 *
 * Externally Read:
 *
 * Externally Modified:
 *
 * Limitations: 
 *
 ***************************************************************************/
void mer_idd_fwd_kin(
    IddPoseP            pose_p, /* pointer to computed pose             */
    merQuaternion *        rvrQtool_p, /* pointer to computed quaternion   */
    const IddJoint      q,      /* joint angles in radians              */
    const IddPrmsDh*    dh_p,   /* pointer to DH parameters             */
    const IddPrmsFrame* frm_p,  /* pointer to rover-to-IDD transform    */
    const IddPrmsTool*  tool_p, /* pointer to tool parameters           */
    IddTool             tool) { /* current tool                         */

    F64     s1, c1, s2, c2, s23, c23, s234, c234, s5, c5, ct, st;
    F64     a1, a2, a3, d1, d4, d5; 

    /* The following variable names violate coding conventions to make them
       more readable */ 
    merMatrix      rvrRidd, iddRtur, turRtool, rvrRtool;  
    merVector      rvrPidd, iddPtur, turPtool, rvrPtool;
    merQuaternion  rvrQidd;

    /* Compute frequently-used trigonometric functions  */  
    s1 = sin(q[0]);
    c1 = cos(q[0]);
    s2 = sin(q[1]);
    c2 = cos(q[1]);
    s23 = sin(q[1]+q[2]);
    c23 = cos(q[1]+q[2]);
    s234 = sin(q[1]+q[2]+q[3]);
    c234 = cos(q[1]+q[2]+q[3]);
    c5 = cos(q[4]);
    s5 = sin(q[4]);
    ct = cos(tool_p[tool].angle);
    st = sin(tool_p[tool].angle);

    /* Extract DH parameters */
    a1 = dh_p[0].length;
    a2 = dh_p[1].length;
    a3 = dh_p[2].length;
    d1 = dh_p[0].offset;
    d4 = dh_p[3].offset;
    d5 = dh_p[4].offset;

    /* Extract the position of the IDD base frame in the rover frame */
    rvrPidd = merVector_Assign(frm_p->x, frm_p->y, frm_p->z);

    /* Compute the orientation of the IDD frame in the rover frame */
    rvrQidd = merQuaternion_Assign(frm_p->q1, frm_p->q2, frm_p->q3,
                                frm_p->q4);
    rvrRidd = meridd_quat_to_rot(rvrQidd);

    /* Compute Cartesian position of the turret in the IDD base frame */
    iddPtur = merVector_Assign(
                a1*c1 + a2*c1*c2 + a3*c1*c23 + d4*s1 - d5*c1*s234,
                a1*s1 + a2*s1*c2 + a3*s1*c23 - d4*c1 - d5*s1*s234,
                d1 + a2*s2 + a3*s23 + d5*c234                      );

    /* Compute the orientation of the turret in the IDD base frame */
    iddRtur = merMatrix_Assign(
        c1*c234*c5 - s1*s5,     -c1*s234,       c1*c234*s5+s1*c5,
        s1*c234*c5 + c1*s5,     -s1*s234,       s1*c234*s5-c1*c5,
        s234*c5,                 c234,          s234*s5           );

    /* Compute Cartesian position of the tool in the turret frame */
    turPtool = merVector_Assign(st*tool_p[tool].length, 0.0,
                             ct*tool_p[tool].length);

    /* Compute the orientation of the tool in the turret frame */
    turRtool = merMatrix_Assign(ct,    0,      st,
                             0,     1,      0,
                            -st,    0,      ct  );

    /* Compute Cartesian position of the tool in the rover frame */
    rvrPtool = merVector_VectorPlusVector(rvrPidd,
                            merVector_MatrixDotVector(rvrRidd, 
                            merVector_VectorPlusVector(iddPtur, 
                            merVector_MatrixDotVector(iddRtur, turPtool))));
    pose_p->x = rvrPtool.v[0];
    pose_p->y = rvrPtool.v[1];
    pose_p->z = rvrPtool.v[2];

    /* Compute the orientaion of the tool in the rover rame */
    rvrRtool = merMatrix_MatrixDotMatrix(rvrRidd,
                        merMatrix_MatrixDotMatrix(iddRtur, turRtool));
    *rvrQtool_p = meridd_rot_to_quat(rvrRtool);
    
    /* Compute the tool azimuth and elevation angles */
    pose_p->az = atan2(rvrRtool.m[1][2], rvrRtool.m[0][2]);
    pose_p->el = atan2(rvrRtool.m[2][2],
                       sqrt(rvrRtool.m[0][2]*rvrRtool.m[0][2] +
                            rvrRtool.m[1][2]*rvrRtool.m[1][2]));
}


/***************************************************************************
 * This is not a copy of FSW code, it is new for the PIG/Mars environment.
 * It sets up the structures with (hard-coded) values for the arm
 * kinematics.  These were derived by calling the real FSW with the real
 * calibration files for each rover and extracting the relevant values
 * (which are constant).  The caller is expected to allocate the storage
 * and pass in pointers to that storage.
 * IddPrmsDh dh[IDD_DOF];
 * IddPrmsFrame frame;
 * IddPrmsTool ptool[IDD_TOOLS];
 * If the string contains "MER2" then Spirit values are used, otherwise Oppy.
 ***************************************************************************/

void mer_idd_setup_kin_params(
    const char *path,
    IddPrmsDh*    dh_p,   /* pointer to DH parameters             */
    IddPrmsFrame* frm_p,  /* pointer to rover-to-IDD transform    */
    IddPrmsTool*  tool_p) /* pointer to tool parameters           */
{
    if (strstr(path, "MER2")) {

	// Spirit parameters

	dh_p[0].length = 0.063869;
	dh_p[0].offset = 0.048089;
	dh_p[0].twist = 1.570796;

	dh_p[1].length = 0.352648;
	dh_p[1].offset = 0;
	dh_p[1].twist = 0;

	dh_p[2].length = 0.329735;
	dh_p[2].offset = 0;
	dh_p[2].twist = 0;

	dh_p[3].length = 0;
	dh_p[3].offset = -0.100183;
	dh_p[3].twist = -1.570796;

	dh_p[4].length = 0;
	dh_p[4].offset = -0.077359;
	dh_p[4].twist = 1.570796;

	frm_p->x = 0.562580;
	frm_p->y = -0.160080;
	frm_p->z = -0.186070;
	frm_p->q1 = 0;
	frm_p->q2 = 0;
	frm_p->q3 = 0;
	frm_p->q4 = 1;

	tool_p[0].tool = MI;
	tool_p[0].length = 0.130600;
	tool_p[0].angle = 0;

	tool_p[1].tool = RAT;
	tool_p[1].length = 0.163600;
	tool_p[1].angle = 1.570796;

	tool_p[2].tool = MB;
	tool_p[2].length = 0.130100;
	tool_p[2].angle = 3.316100;

	tool_p[3].tool = APXS;
	tool_p[3].length = 0.129300;
	tool_p[3].angle = 4.712400;

	tool_p[4].tool = IDD_TURRET;
	tool_p[4].length = 0;
	tool_p[4].angle = 0;

	tool_p[5].tool = IDD_STOW_T;
	tool_p[5].length = 0.031400;
	tool_p[5].angle = 0.586300;

    } else {

	// Opportunity parameters

	dh_p[0].length = 0.062443;
	dh_p[0].offset = 0.048494;
	dh_p[0].twist = 1.570796;

	dh_p[1].length = 0.353552;
	dh_p[1].offset = 0;
	dh_p[1].twist = 0;

	dh_p[2].length = 0.330349;
	dh_p[2].offset = 0;
	dh_p[2].twist = 0;

	dh_p[3].length = 0;
	dh_p[3].offset = -0.098924;
	dh_p[3].twist = -1.570796;

	dh_p[4].length = 0;
	dh_p[4].offset = -0.077131;
	dh_p[4].twist = 1.570796;

	frm_p->x = 0.562450;
	frm_p->y = -0.159900;
	frm_p->z = -0.186540;
	frm_p->q1 = 0;
	frm_p->q2 = 0;
	frm_p->q3 = 0;
	frm_p->q4 = 1;

	tool_p[0].tool = MI;
	tool_p[0].length = 0.130600;
	tool_p[0].angle = 0;

	tool_p[1].tool = RAT;
	tool_p[1].length = 0.163600;
	tool_p[1].angle = 1.570796;

	tool_p[2].tool = MB;
	tool_p[2].length = 0.130100;
	tool_p[2].angle = 3.316100;

	tool_p[3].tool = APXS;
	tool_p[3].length = 0.129300;
	tool_p[3].angle = 4.712400;

	tool_p[4].tool = IDD_TURRET;
	tool_p[4].length = 0;
	tool_p[4].angle = 0;

	tool_p[5].tool = IDD_STOW_T;
	tool_p[5].length = 0.031400;
	tool_p[5].angle = 0.586300;
    }
}

