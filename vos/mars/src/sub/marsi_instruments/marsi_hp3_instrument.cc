#include "marsi_hp3_instrument.h"

#include <cfloat>
#include <cmath>
#include <iostream>
#include <cassert>
#include "zvproto.h"


/***********************************************************************/

    // The plot_* routines all create vectors of circles representing the
    // instruments.  They are internal to here and should be accessed
    // only via readInstrumentShape().

/***********************************************************************/

  void PigNsytHP3_InstrumentParams::plot_inst_feet(
		std::vector<SampleCircle> &circles) const
{
    plot_hp3_narrow_feet_pair(circles);
    plot_hp3_wide_feet_pair(circles);
}


  void PigNsytHP3_InstrumentParams::plot_hp3_body(std::vector<SampleCircle> &circles)
  {
    for (auto hp3_radius_index = -hp3_patch_radius_num_negative_increments;
         hp3_radius_index <= hp3_patch_radius_num_positive_increments;
         hp3_radius_index++)
    {
      const auto hp3_radius = hp3_radius_index * hp3_patch_radius_delta_meters;

      add_circle(
        circles,
        hp3_radius, 0.,
        hp3_feet_patch_radius_meters,
        global_body_max_height);
    }
  }

/***********************************************************************/

  void PigNsytHP3_InstrumentParams::plot_hp3_feet_pair(
		std::vector<SampleCircle> &circles,
                                 const double offset_phi,
                                 const double location_radius_meters,
                                 const double foot_radius) const
  {
    for (int sign = -1; sign <= 1; sign++)
    {
      if (sign == 0)
      {
        continue;
      }

      const auto foot_phi = sign * offset_phi;

      add_circle(
        circles,
        location_radius_meters, foot_phi,
        foot_radius,
        global_feet_max_height);
    }
  }

/***********************************************************************/

  void PigNsytHP3_InstrumentParams::plot_hp3_wide_feet_pair(
		std::vector<SampleCircle> &circles) const
  {
    plot_hp3_feet_pair(circles,
                       radians(hp3_wide_feet_patch_offset_phi_degrees),
                       hp3_wide_feet_foot_location_radius_meters,
                       hp3_feet_patch_radius_meters);
  }

/***********************************************************************/

  void PigNsytHP3_InstrumentParams::plot_hp3_narrow_feet_pair(
		std::vector<SampleCircle> &circles) const
  {
    plot_hp3_feet_pair(circles,
                      radians(hp3_narrow_feet_patch_offset_phi_degrees),
                      hp3_narrow_feet_foot_location_radius_meters,
                      hp3_feet_patch_radius_meters);
  }

/***********************************************************************/

  void PigNsytHP3_InstrumentParams::plot_hp3_drill(
		std::vector<SampleCircle> &circles)
  {
    add_circle(
      circles,
      hp3_drill_foot_location_radius_meters, 0.,
      hp3_drill_foot_radius_meters,
      global_feet_max_height);
  }

/***********************************************************************/

  std::vector<SampleCircle> PigNsytHP3_InstrumentParams::plot_hp3_footpatches()
  {
    std::vector<SampleCircle> circles;

    plot_hp3_narrow_feet_pair(circles);
    plot_hp3_wide_feet_pair(circles);
    plot_hp3_drill(circles);

    return circles;
  }

/***********************************************************************/

  std::vector<SampleCircle> PigNsytHP3_InstrumentParams::plot_hp3_footplanes()
  {
    std::vector<SampleCircle> circles;

    plot_hp3_body(circles);

    return circles;
  }









/***********************************************************************
 * Rotate a set of circles for a given instrument.  The xy position is
 * used to calculate what the rotation should be (based on the tether),
 * and the clock angle IN DEGREES is then added to that.
 * rotateByAngle() is then called to do the rotation.
 ***********************************************************************/

  std::vector<SampleCircle> PigNsytHP3_InstrumentParams::rotateInstrument(
		const std::vector<SampleCircle> &circles,
		const double x, const double y,
		const double clock) const
  {

    double angle;

    angle = hp3_rotation(x, y);

    angle += PigDeg2Rad(clock);

    return rotateByAngle(circles, angle);

  }


/***********************************************************************
 * Figure out the HP3 rotation due to the tether

    * From Won Kim:
    * HP3 has two "virtual" tether anchor points.
    *   - HP3 tether guard post1 --- P1 = (X1, Y1) = (0.083, -0.435)
    *   - HP3 mount --- P2 = (X2, Y2) = (-1.279, 0.149)
    *
    *   - Line equation between two anchor points:
    *     y = ((Y2-Y1)/(X2-X1)) (x - X1) + Y1 = mx + b
    *   - Given the current tether position (X0, Y0):
    *     - If Y0 > m * X0 + b, use HP3 tether guard post1 P1 as the "virtual" anchor
    *       point.
    *     - Otherwise, use HP3 mount P2 as the "virtual" anchor point.
    *
    * You can determine the tether direction by assuming that the tether passes
    * through the "virtual" anchor point.
    *
    * To determine the WTS orientation, use a "virtual" tether from the WTS to the
    * "virtual" anchor point, which is at the IDA arm base --- (X, Y) = (0, 0).
 ***********************************************************************/

  double PigNsytHP3_InstrumentParams::hp3_rotation(const double x,
						const double y) const
  {
    const auto m =  (hp3_y2 - hp3_y1) / (hp3_x2 - hp3_x1);
    const auto mx_plus_b = m * (x - hp3_x1) + hp3_y1;

    const auto first = y > mx_plus_b;

    return atan2(y - (first ? hp3_y1 : hp3_y2),
                 x - (first ? hp3_x1 : hp3_x2));
  }

/***********************************************************************
 * Read the given instrument shape
 * Replace with reading from files eventually
 ***********************************************************************/

void PigNsytHP3_InstrumentParams::readInstrumentShape(InstrumentShape &shape,
			 RoughnessType type)
{
    readInstrumentParams();

    if (type == INST_BODY) {
        shape.circles = plot_hp3_footplanes();
    } else if (type == INST_FEET) {
        shape.circles = plot_hp3_footpatches();
    } else {
        zvmessage("WARNING!  Invalid HP3 Instrument Type", "");
        zabend();
    }
}


/***********************************************************************
 * Fill up the InstrumentParams structure
 * !!!! This really should be read from a config file.   Constants for now.
 ***********************************************************************/

#include "nsyt_foot_dimensions.h"

void PigNsytHP3_InstrumentParams::readInstrumentParams()
{
    //!!!! This first set was defined in the .cc file rather than the .h
    global_feet_max_height = .01;
    global_body_max_height = .03;
    global_wts_body_max_height = .06;
    hp3_x1 = .083;
    hp3_y1 = -.435;
    hp3_x2 = -1.266;
    hp3_y2 = .147;

    // Everything below is in nsyt_foot_dimensions.h

    hp3_feet_patch_radius_meters = HP3_FEET_PATCH_RADIUS_METERS;
    hp3_wide_feet_foot_location_radius_meters = HP3_WIDE_FEET_FOOT_LOCATION_RADIUS_METERS;
    hp3_narrow_feet_foot_location_radius_meters = HP3_NARROW_FEET_FOOT_LOCATION_RADIUS_METERS;
    hp3_drill_foot_location_radius_meters = HP3_DRILL_FOOT_LOCATION_RADIUS_METERS;
    hp3_drill_foot_radius_meters = HP3_DRILL_FOOT_RADIUS_METERS;
    hp3_wide_feet_patch_offset_phi_degrees = HP3_WIDE_FEET_PATCH_OFFSET_PHI_DEGREES;
    hp3_narrow_feet_patch_offset_phi_degrees = HP3_NARROW_FEET_PATCH_OFFSET_PHI_DEGREES;
    hp3_body_delta_degrees = HP3_BODY_DELTA_DEGREES;
    hp3_body_num_increments = HP3_BODY_NUM_INCREMENTS;
    hp3_patch_radius_delta_meters = HP3_PATCH_RADIUS_DELTA_METERS;
    hp3_patch_radius_num_positive_increments = HP3_PATCH_RADIUS_NUM_POSITIVE_INCREMENTS;
    hp3_patch_radius_num_negative_increments = HP3_PATCH_RADIUS_NUM_NEGATIVE_INCREMENTS;

}

