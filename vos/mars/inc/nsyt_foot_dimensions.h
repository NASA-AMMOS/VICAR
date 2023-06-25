#ifndef LOAD_INST_H
#define LOAD_INST_H


/**!!!! THE FOLLOWING SHOULD ALL BE READ FROM THE CONFIG FILE !!!!*/

/** Radius of SEIS.
 */
#define SEIS_RADIUS_METERS 0.20654

/** Radius at which the feet are located.
 */
#define SEIS_FOOT_LOCATION_RADIUS_METERS 0.125

/** Offset from x axis to middle of first foot patch.
 * There are three equally spaced feet under SEIS.
 */
#define SEIS_FIRST_PATCH_OFFSET_PHI_DEGREES (180+22.0)

/** Radius of foot patch. This is used by the below.
 */
#define SEIS_PATCH_RADIUS_METERS 0.03


/** Radius of SEIS tether patch. This is used by the below.
 */
#define SEIS_TETHER_PATCH_RADIUS_METERS 0.03

/** We iteratively plot circles across the tether's length.
 *
 * SEIS_TETHER_PATCH_RADIUS_DELTA_METERS defines the offset between each
 * circle.
 */
#define SEIS_TETHER_PATCH_RADIUS_DELTA_METERS 0.03

// TODO: Define in terms of meters instead.
#define SEIS_TETHER_PATCH_RADIUS_NUM_INCREMENTS_START 6
#define SEIS_TETHER_PATCH_RADIUS_NUM_INCREMENTS_FINISH 16

/** Offset from x axis to middle of tether's foot patch.
 */
#define SEIS_TETHER_OFFSET_PHI_DEGREES 0


/** Radius of WTS.
 */
#define WTS_RADIUS_METERS 0.3512

/** Radius along which WTS foot patches lie.
 */
#define WTS_FOOT_LOCATION_RADIUS_METERS 0.4762

/** Offset from x axis to middle of first foot patch.
 */
#define WTS_FIRST_PATCH_OFFSET_PHI_DEGREES  (-27.75+180)
#define WTS_SECOND_PATCH_OFFSET_PHI_DEGREES (WTS_FIRST_PATCH_OFFSET_PHI_DEGREES + 130.)
#define WTS_THIRD_PATCH_OFFSET_PHI_DEGREES  (WTS_SECOND_PATCH_OFFSET_PHI_DEGREES + 105.)

#if 0
/** We rotate the circle about the center of WTS to create a foot patch.
 * This delta defines the precision at which we rotate. Note that this
 * approach creates scalloped edges.
 */
#define WTS_PATCH_SUBSET_DELTA_DEGREES 4.375

/** We rotate the circle this plus or minus this number of iterations
 * at WTS_PATCH_SUBSET_DELTA_DEGREES increments to create the foot patch.
 */
#define WTS_PATCH_SUBSET_NUM_INCREMENTS 4
#endif

/** Radius of foot patch.
 */
#define WTS_PATCH_RADIUS_METERS 0.028


/** Radius of HP3 patch. This is used by the below.
 */
#define HP3_FEET_PATCH_RADIUS_METERS 0.04
#define HP3_DRILL_FOOT_RADIUS_METERS 0.029

#define HP3_WIDE_FEET_FOOT_LOCATION_RADIUS_METERS 0.33826
#define HP3_NARROW_FEET_FOOT_LOCATION_RADIUS_METERS 0.18338
#define HP3_DRILL_FOOT_LOCATION_RADIUS_METERS 0.09

#define HP3_WIDE_FEET_PATCH_OFFSET_PHI_DEGREES (180-29.98)
#define HP3_NARROW_FEET_PATCH_OFFSET_PHI_DEGREES 25.87

// FIXME: Get rid of this. Derive this from the feet parameters.
#define HP3_BODY_DELTA_DEGREES 10
#define HP3_BODY_NUM_INCREMENTS 2

// FIXME: Get rid of this. Derive this from the feet parameters.
/** We iteratively plot circles across the HP3's length.
  - *
  - * HP3_PATCH_RADIUS_DELTA_METERS defines the offset between each
  - * circle.  Circles should be closer together than half the radius
  - * of the circle, in order to minimize the scalloping of the region.
  - *
  - * HP3_PATCH_RADIUS_NUM_POSITIVE_INCREMENTS defines the number of
  - * iterations we plot in the positive direction.
  - *
  - * HP3_PATCH_RADIUS_NUM_NEGATIVE_INCREMENTS defines the number of
  - * iterations we plot in the negative direction.
  - */
#if 0
// original, circles too far apart
#define HP3_PATCH_RADIUS_DELTA_METERS 50e-3
#define HP3_PATCH_RADIUS_NUM_POSITIVE_INCREMENTS 4
#define HP3_PATCH_RADIUS_NUM_NEGATIVE_INCREMENTS 6
#else
// revised, half the delta for the circles
#define HP3_PATCH_RADIUS_DELTA_METERS 12.5e-3
#define HP3_PATCH_RADIUS_NUM_POSITIVE_INCREMENTS 16
#define HP3_PATCH_RADIUS_NUM_NEGATIVE_INCREMENTS 24
#endif

#endif
