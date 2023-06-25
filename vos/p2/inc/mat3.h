/******************************************************************************
*                                                                             *
*                                     M A T 3                                 *
*                                                                             *
*                                       Todd Litwin                           *
*                                       Written: 12 Sep 1991                  *
*                                       Updated: 19 Feb 2016                  *
*                                                                             *
*******************************************************************************


	This is the header file for the MAT3 functions. */

#ifndef _MAT3_H
#define _MAT3_H

#ifdef  MAT3_FLOAT
typedef MAT3_FLOAT float3;
#else
typedef double     float3;
#endif

#ifdef	__cplusplus
extern "C" {
#endif

#ifdef __STDC__

/******************************************************************************

    This function adds two 3-vectors. Returns pointer to output vector. */

float3 *add3(
    const float3 a[3],  /* input addend vector */
    const float3 b[3],  /* input addend vector */
    float3 c[3]);       /* output sum vector */

/******************************************************************************

    This function adds together two 3x3 matrices. Returns pointer to output
    matrix. */

float3 (*add33(
    float3 a[3][3],	/* input addend matrix */
    float3 b[3][3],	/* input addend matrix */
    float3 c[3][3]	/* output sum matrix */
    ))[3];

/******************************************************************************

    This function copies a 3-vector. Returns pointer to output vector. */

float3 *copy3(
    const float3 a[3],	/* input vector */
    float3 b[3]);	/* output vector */

/******************************************************************************

    This function copies a 3x3 matrix. Returns pointer to output matrix. */

float3 (*copy33(
    float3 a[3][3],	/* input matrix */
    float3 b[3][3]	/* output matrix */
    ))[3];

/******************************************************************************

    This function copies a quaternion (a 4-vector). Returns pointer to output
    quaternion. */

float3 *copyq(
    const float3 a[4],	/* input quaternion */
    float3 b[4]);	/* output quaternion */

/******************************************************************************

    This function performs the cross product of two 3-vectors. Returns pointer
    to output vector. */

float3 *cross3(
    const float3 a[3],	/* input vector */
    const float3 b[3],	/* input vector */
    float3 c[3]);	/* output vector */

/******************************************************************************

    This function computes the determinant of a 3x3 matrix. */

float3 det33(
    float3 a[3][3]);	/* input matrix */

/******************************************************************************

    This function computes the inner product of two 3-vectors. */

float3 dot3(
    const float3 a[3],	/* input vector */
    const float3 b[3]);	/* input vector */

/******************************************************************************

    This function computes the derivative of the rotation matrix that could
    be constructed from the given vector and angle of rotation about that
    vector. It is built for pre-multiplication. See quatva() and rotq().
    Returns pointer to output matrix. */

float3 (*drotva(
    const float3 v[3],	/* input vector */
    float3 a,		/* input angle of rotation */
    float3 drda[3][3]	/* output derivative matrix */
    ))[3];

/******************************************************************************

    This function creates a 3x3 identity matrix. Returns pointer to output
    matrix. */

float3 (*ident33(
    float3 a[3][3]	/* output matrix */
    ))[3];

/******************************************************************************

    This function creates an identity quaternion. Returns pointer to output
    quaternion. */

float3 *identq(
    float3 a[4]);	/* output quaternion */

/******************************************************************************

    This function inverts any invertible 3x3 matrix. Returns pointer to output
    matrix. */

float3 (*inv33(
    float3 a[3][3],	/* input matrix */
    float3 b[3][3]	/* output matrix */
    ))[3];

/******************************************************************************

    This function inverts a positive-definite 3x3 matrix. Since the function
    inv33() is faster and is not restricted to positive-definite matrices,
    it is recommended over this one. The only reason for putting inv33pd()
    here is as an example of how to invert larger matrices. The recursive
    method used by inv33() does not scale nicely, and quickly becomes too
    slow. But this function scales up quite well. Returns pointer to output
    matrix. */

float3 (*inv33pd(
    float3 a[3][3],	/* input matrix */
    float3 b[3][3]	/* output matrix */
    ))[3];

/******************************************************************************

    This function inverts a quaternion. It assumes that quaternion is
    normalized. [See unitq() to normalize.] Returns pointer to output
    quaternion. */

float3 *invq(
    const float3 a[4],	/* input quaternion */
    float3 b[4]);	/* output quaternion */

/******************************************************************************

    This function computes the magnitude of a 3-vector. */

float3 mag3(
    const float3 a[3]); /* input vector */

/******************************************************************************

    This function multiplies a 3-vector (1x3 matrix) by a 3x3 matrix. Returns
    pointer to output matrix. */

float3 *mult133(
    const float3 a[3],	/* input vector */
    float3 b[3][3],	/* input matrix */
    float3 c[3]);	/* output vector */

/******************************************************************************

    This function multiplies a 3-vector (3x1 matrix) by a 3-vector
    (1x3 matrix). Returns pointer to output matrix. */

float3 (*mult313(
    const float3 a[3],	/* input vector */
    const float3 b[3],	/* input vector */
    float3 c[3][3]	/* output matrix */
    ))[3];

/******************************************************************************

    This function multiplies a 3x3 matrix by a 3-vector (3x1 matrix). Returns
    pointer to output vector. */

float3 *mult331(
    float3 m[3][3],	/* input matrix */
    const float3 v[3],	/* input vector */
    float3 u[3]);	/* output vector */

/******************************************************************************

    This function multiplies two 3x3 matrices. Returns pointer to output
    matrix. */

float3 (*mult333(
    float3 a[3][3],	/* input matrix */
    float3 b[3][3],	/* input matrix */
    float3 c[3][3]	/* output matrix */
    ))[3];

/******************************************************************************

    This function multiplies two quaternions together. Returns pointer to
    output quaternion. */

float3 *multq(
    const float3 a[4],	/* input rotation quaternion */
    const float3 b[4],	/* input orientation quaternion */
    float3 c[4]);	/* output orientation quaternion */

/******************************************************************************

    This function rotates a vector by a quaternion. To do so it premultiplies
    by the quaternion and postmultiplies by the inverse of the quaternion.
    The calculation used was derived by the author from first principles (see
    Wikipedia on Quaternions), and so is suspect. Returns pointer to output
    vector. */

float3 *multqv(
    const float3 q[4],	/* input rotation quaternion */
    const float3 v[3],	/* input vector */
    float3 u[3]);	/* output vector */

/******************************************************************************

    This function converts a rotation matrix to an equivalent unit quaternion
    which represents rotation. Returns pointer to output quaternion. */

float3 *quatr(
    float3 r[3][3],	/* input rotation matrix */
    float3 q[4]);	/* output quaternion */

/******************************************************************************

    This function converts a vector and an angle of rotation about that vector
    to an equivalent unit quaternion which represents rotation of an object
    in a fixed coordinate system. It is constructed for pre-multiplication.
    Returns pointer to output quaternion. */

float3 *quatva(
    const float3 v[3],	/* input vector */
    float3 a,		/* input angle of rotation */
    float3 q[4]);	/* output quaternion */

/******************************************************************************

    This function converts three axis rotations into a quaternion. The
    rotations are the familiar roll, pitch, and yaw (heading), performed on
    an object about the static world coordinates. This is the inverse of the
    Euler angles about the Z axis, followed by the rotated Y axis, followed
    by the twice-rotated X axis. Returns pointer to output quaternion. */

float3 *quatxyz(
    float3 a,		/* input angle about X axis (roll) */
    float3 b,		/* input angle about Y axis (pitch) */
    float3 c,		/* input angle about Z axis (yaw) */
    float3 q[4]);	/* output quaternion */

/******************************************************************************

    This function converts a unit quaternion which represents rotation to
    an equivalent rotation matrix. Returns pointer to output matrix. */

float3 (*rotq(
    const float3 q[4],	/* input quaternion */
    float3 r[3][3]	/* output rotation matrix */
    ))[3];

/******************************************************************************

    This function converts three axis rotations into a rotation matrix. The
    rotations are the familiar roll, pitch, and yaw (heading), performed on
    an object about the static world coordinates. This is the inverse of the
    Euler angles about the Z axis, followed by the rotated Y axis, followed
    by the twice-rotated X axis. This function was constructed from the
    transpose, which is the inverse in this case, of the equations found
    in Herbert Goldstein, "Classical Mechanics," second edition, p. 609,
    called there the "xyz convention." Returns pointer to output matrix. */

float3 (*rotxyz(
    float3 a,		/* input angle about X axis (roll) */
    float3 b,		/* input angle about Y axis (pitch) */
    float3 c,		/* input angle about Z axis (yaw) */
    float3 r[3][3]	/* output rotation matrix */
    ))[3];

/******************************************************************************

    This function converts three axis rotations into a rotation matrix. The
    rotations are as performed on an object about the static world Z, then
    X, then Z coordinates. This is the inverse of the Euler angles about the
    Z axis, followed by the rotated X axis, followed by the rotated Z axis.
    This function was constructed from the transpose, which is the inverse in
    this case, of the equations found in Herbert Goldstein, "Classical
    Mechanics," second edition, p. 147, called there the "x convention."
    Returns pointer to output matrix. */

float3 (*rotzxz(
    float3 a,		/* input angle about original Z axis */
    float3 b,		/* input angle about rotated  X axis */
    float3 c,		/* input angle about rotated  Z axis */
    float3 r[3][3]	/* output rotation matrix */
    ))[3];

/******************************************************************************

    This function converts three axis rotations into a rotation matrix. The
    rotations are as performed on an object about the static world Z, then
    Y, then Z coordinates. This is the inverse of the Euler angles about the
    Z axis, followed by the rotated Y axis, followed by the rotated Z axis.
    This function was constructed from the transpose, which is the inverse in
    this case, of the equations found in Herbert Goldstein, "Classical
    Mechanics," second edition, p. 147, called there the "y convention."
    Returns pointer to output matrix. */

float3 (*rotzyz(
    float3 a,		/* input angle about original Z axis */
    float3 b,		/* input angle about rotated  Y axis */
    float3 c,		/* input angle about rotated  Z axis */
    float3 r[3][3]	/* output rotation matrix */
    ))[3];

/******************************************************************************

    This function multipies a 3-vector by a scalar. Returns pointer to output
    vector. */

float3 *scale3(
    float3 s,		/* input scalar */
    const float3 a[3],	/* input vector */
    float3 b[3]);	/* output vector */

/******************************************************************************

    This function multipies a 3x3 matrix by a scalar. Returns pointer to output
    matrix. */

float3 (*scale33(
    float3 s,		/* input scalar */
    float3 a[3][3],	/* input matrix */
    float3 b[3][3]	/* output matrix */
    ))[3];

/******************************************************************************

    This function subtracts two 3-vectors. Returns pointer to output vector. */

float3 *sub3(
    const float3 a[3],	/* input minuend vector */
    const float3 b[3],	/* input subtrahend vector */
    float3 c[3]);	/* output difference vector */

/******************************************************************************

    This function subtracts two 3x3 matrices. Returns pointer to output matrix.
    */

float3 (*sub33(
    float3 a[3][3],	/* input minuend matrix */
    float3 b[3][3],	/* input subtrahend matrix */
    float3 c[3][3]	/* output difference matrix */
    ))[3];

/******************************************************************************

    This function transposes a 3x3 matrix. Returns pointer to output matrix. */

float3 (*trans33(
    float3 a[3][3],	/* input matrix */
    float3 b[3][3]	/* output matrix */
    ))[3];

/******************************************************************************

    This function normalizes a 3-vector, producing a unit 3-vector. Returns
    pointer to output vector, or NULL on error. */

float3 *unit3(
    const float3 a[3],	/* input vector */
    float3 b[3]);	/* output vector */

/******************************************************************************

    This function normalizes a quaternion, producing a unit quaternion. It
    returns a pointer to the result, or NULL on error. */

float3 *unitq(
    const float3 a[4],	/* input quaternion */
    float3 b[4]);	/* output quaternion */

/******************************************************************************

    This function converts a quaternion into a vector and an angle or rotation
    about that vector. If the quaternion is a unit quaternion, then the output
    vector will be a unit vector. See quatva() for comments on representation.
    Returns output angle. */

float3 vaquat(
    const float3 q[4],	/* input quaternion */
    float3 v[3],	/* output vector */
    float3 *a);		/* output angle of rotation */

/******************************************************************************

    This function converts a quaternion into three axis rotations. The
    rotations are the familiar roll, pitch, and yaw (heading), performed on
    an object about the static world coordinates. This is the inverse of the
    Euler angles about the Z axis, followed by the rotated Y axis, followed
    by the twice-rotated X axis. See rotxyz(). Returns pointer to input
    quaternion. */

const float3 *xyzquat(
    const float3 q[4],	/* input quaternion */
    float3 *a,		/* output angle about X axis (roll) */
    float3 *b,		/* output angle about Y axis (pitch) */
    float3 *c);		/* output angle about Z axis (yaw) */

/******************************************************************************

    This function converts a rotation matrix into three axis rotations. The
    rotations are the familiar roll, pitch, and yaw (heading), performed on
    an object about the static world coordinates. This is the inverse of the
    Euler angles about the Z axis, followed by the rotated Y axis, followed
    by the twice-rotated X axis. See rotxyz(). Returns pointer to input
    quaternion. */

float3 (*xyzrot(
    float3 r[3][3],	/* input rotation matrix */
    float3 *a,		/* output angle about X axis (roll) */
    float3 *b,		/* output angle about Y axis (pitch) */
    float3 *c		/* output angle about Z axis (yaw) */
    ))[3];

/******************************************************************************

    This function creates a zero 3-vector. Returns pointer to output vector. */

float3 *zero3(
    float3 a[3]);	/* output vector */

/******************************************************************************

    This function creates a zero 3x3 matrix. Returns pointer to output matrix.
    */

float3 (*zero33(
    float3 a[3][3]	/* output matrix */
    ))[3];

/******************************************************************************

    This function creates a zero quaternion, that is, one with all elements
    zero. This is not a usable quaternion. See identq() for one that represents
    a rotation by a zero angle. Returns pointer to output quaternion. */

float3 *zeroq(
    float3 a[4]);	/* output quaternion */

/******************************************************************************

    This function converts a rotation matrix into three axis rotations. The
    rotations are as performed on an object about the static world Z, then
    X, then Z coordinates. This is the inverse of the Euler angles about the
    Z axis, followed by the rotated X axis, followed by the rotated Z axis.
    See rotzxz(). Note that this operation becomes ill-conditioned as the X
    term (b) approaches zero. */

float3 (*zxzrot(
    float3 r[3][3],	/* input rotation matrix */
    float3 *a,		/* output angle about original Z axis */
    float3 *b,		/* output angle about rotated  X axis */
    float3 *c		/* output angle about rotated  Z axis */
    ))[3];

/*****************************************************************************/

#else

float3  *add3();	/* vector addition: A+B -> C */
float3 (*add33())[3];	/* matrix addition: A+B -> C */
float3  *copy3();	/* vector copy: A -> B */
float3 (*copy33())[3];	/* matrix copy: A -> B */
float3  *copyq();	/* quaternion copy: A -> B */
float3  *cross3();	/* vector cross product: AxB -> C */
float3   det33();	/* matrix determinant: |A| */
float3   dot3();	/* vector inner (dot) product: A.B */
float3 (*drotva())[3];	/* rot-matrix deriv from vect/angle: f(V,a) -> dR/da */
float3 (*ident33())[3];	/* matrix identity: I -> A */
float3  *identq();	/* quaternion identity: I -> A */
float3 (*inv33())[3];	/* matrix inversion (general): 1/A -> B */
float3 (*inv33pd())[3];	/* matrix inversion (positive-definite): 1/A -> B */
float3  *invq();	/* quaternion inversion: 1/A -> B */
float3   mag3();	/* vector magnitude: |A| */
float3  *mult133();	/* vector-matrix multiply: AB -> C */
float3 (*mult313())[3];	/* vector-vector multiply: AB -> C */
float3  *mult331();	/* matrix-vector multiply: MV -> U */
float3 (*mult333())[3];	/* matrix-matrix multiply: AB -> C */
float3  *multq();	/* quaternion-quaternion multiply: AB -> C */
float3  *multqv();	/* quaternion-vector multiply: QV(1/Q) -> U */
float3  *quatr();	/* quaternion from rotation matrix: f(R) -> Q */
float3  *quatva();	/* quaternion from vector & angle: f(V,a) -> Q */
float3  *quatxyz();	/* quaternion from Euler XYZ: f(a,b,c) -> Q */
float3 (*rotq())[3];	/* rotation matrix from quaternion: f(Q) -> R */
float3 (*rotxyz())[3];	/* rotation matrix from Euler XYZ: f(a,b,c) -> R */
float3 (*rotzxz())[3];	/* rotation matrix from Euler ZXZ: f(a,b,c) -> R */
float3 (*rotzyz())[3];	/* rotation matrix from Euler ZYZ: f(a,b,c) -> R */
float3  *scale3();	/* vector scaling: sA -> B */
float3 (*scale33())[3];	/* matrix scaling: sA -> B */
float3  *sub3();	/* vector subtraction: A-B -> C */
float3 (*sub33())[3];	/* matrix subtraction: A-B -> C */
float3 (*trans33())[3];	/* matrix transpose: At -> B */
float3  *unit3();	/* vector normalize: A/|A| -> B */
float3  *unitq();	/* quaternion normalize: f(A) -> B */
float3   vaquat();	/* vector & angle from quaternion: f(Q) -> V,a */
const
 float3 *xyzquat();	/* Euler XYZ from quaternion: q -> a,b,c */
float3 (*xyzrot())[3];	/* Euler XYZ from rotation matrix: r -> a,b,c */
float3  *zero3();	/* vector zero: 0 -> A */
float3 (*zero33())[3];	/* matrix zero: 0 -> A */
float3  *zeroq();	/* quaternion zero: 0 -> A */
float3 (*zxzrot())[3];	/* Euler ZXZ from rotation matrix: r -> a,b,c */

#endif

#ifdef	__cplusplus
}
#endif

#endif
