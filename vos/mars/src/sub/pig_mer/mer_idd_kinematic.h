#ifndef _mer_idd_kinematic_h_
#define _mer_idd_kinematic_h_

/* This originally came from the MER FSW.  We extracted just what we need
 * for the PIG library here, to avoid linking to the FSW.  This file contains
 * code mostly from idd_kinematics.h and Matrix.h.
 */

typedef double    F64;

#define IDD_DOF     5
#define IDD_TOOLS   6

typedef struct _merQuaternion {
    double q[4];
} merQuaternion;

typedef enum {      /* numbers must stay as assigned                */
    MI = 0,         /* Microscopic Imager                           */
    RAT,            /* Rock Abrasion Tool                           */
    MB,             /* Mossbauer Spectrometer                       */
    APXS,           /* Alpha Particle X-ray Spectrometer            */
    IDD_TURRET,     /* Origin of the IDD turret frame (IDD5)        */
    IDD_STOW_T      /* Turret stow T-bar                            */
} IddTool;          /* must be in order above                       */


typedef struct {
    F64     x;      /* x coordinate in specified frame (m)          */
    F64     y;      /* y coordinate in specified frame (m)          */
    F64     z;      /* z coordinate in specified frame (m)          */
    F64     az;     /* azimuth angle in specified frame (radians)   */
    F64     el;     /* elevation angle in specified frame (radians) */
} IddPose, *IddPoseP;

typedef F64 IddJoint[IDD_DOF];

typedef struct {
    F64 length;     /* distance along the x_i axis from the         */
                    /* intersection of the x_i and z_(i-1) axes to  */
                    /* the origin of the ith frame                  */
    F64 offset;     /* distance along the z_(i-1) axis from the     */
                    /* origin of (i-1)th frame to the intersection  */
                    /* of the x_i and z_(i-1) axes                  */
    F64 twist;      /* angle from the z_(i-1) axis to the z_i axis  */
                    /* about the x_i axis                           */
} IddPrmsDh;


typedef struct {
    F64 x;          /* x coordinate of the IDD base frame in the    */
                    /* rover frame                                  */
    F64 y;          /* y coordinate of the IDD base frame in the    */
                    /* rover frame                                  */
    F64 z;          /* z coordinate of the IDD base frame in the    */
                    /* rover frame                                  */
    F64 q1;         /* 1st element of the quaternion representing   */
                    /* orientation of the IDD base frame in the     */
                    /* rover frame                                  */
    F64 q2;         /* 2nd element of the quaternion representing   */
                    /* orientation of the IDD base frame in the     */
                    /* rover frame                                  */
    F64 q3;         /* 3rd element of the quaternion representing   */
                    /* orientation of the IDD base frame in the     */
                    /* rover frame                                  */
    F64 q4;         /* 4th element of the quaternion representing   */
                    /* orientation of the IDD base frame in the     */
                    /* rover frame                                  */
} IddPrmsFrame;

typedef struct {
    IddTool tool;   /* tool designator                              */
    F64     length; /* the distance from the origin of the turret   */
                    /* frame to the origin of the tool frame        */
    F64     angle;  /* angle from the z_5 axis to z_tool axis about */
                    /* the y_5 axis                                 */
} IddPrmsTool;


void mer_idd_fwd_kin(
    IddPoseP            pose_p, /* pointer to computed pose             */
    merQuaternion *        rvrQtool_p, /* pointer to computed quaternion   */
    const IddJoint      q,      /* joint angles in radians              */
    const IddPrmsDh*    dh_p,   /* pointer to DH parameters             */
    const IddPrmsFrame* frm_p,  /* pointer to rover-to-IDD transform    */
    const IddPrmsTool*  tool_p, /* pointer to tool parameters           */
    IddTool             tool);  /* current tool                         */

void mer_idd_setup_kin_params(
    const char *path,
    IddPrmsDh*    dh_p,   /* pointer to DH parameters             */
    IddPrmsFrame* frm_p,  /* pointer to rover-to-IDD transform    */
    IddPrmsTool*  tool_p); /* pointer to tool parameters          */


#endif

