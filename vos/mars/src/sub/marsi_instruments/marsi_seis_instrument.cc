#include "marsi_seis_instrument.h"

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

  void PigNsytSeis_InstrumentParams::plot_seis_tether(
		std::vector<SampleCircle> &circles)
  {
    const auto offset_phi = radians(seis_tether_offset_phi_degrees);

    for (int tether_radius_index = -seis_tether_patch_radius_num_increments_finish;
         tether_radius_index <= -seis_tether_patch_radius_num_increments_start;
         tether_radius_index++)
    {
      const double tether_radius =
        tether_radius_index * seis_tether_patch_radius_delta_meters;

      add_circle(
        circles,
        tether_radius, offset_phi,
        seis_patch_radius_meters,
        global_feet_max_height);
    }
  }

/***********************************************************************/

  void PigNsytSeis_InstrumentParams::plot_inst_feet(
		std::vector<SampleCircle> &circles) const
  {
    const auto offset_phi = radians(seis_first_patch_offset_phi_degrees);

    for (size_t foot_index = 0; foot_index < num_seis_feet; foot_index++)
    {
      const auto foot_phi =
        offset_phi +
        radians(foot_index * 360. / num_seis_feet);

      add_circle(
        circles,
        seis_foot_location_radius_meters, foot_phi,
        seis_patch_radius_meters,
        global_feet_max_height);
    }
  }

/***********************************************************************/

 std::vector<SampleCircle> PigNsytSeis_InstrumentParams::plot_seis_footpatches()
  {
    std::vector<SampleCircle> circles;

    plot_inst_feet(circles);
    //!!!! DISABLED TETHER because it would be way outside any image
    //!!!!plot_seis_tether(circles);

    return circles;
  }

/***********************************************************************/

  std::vector<SampleCircle> PigNsytSeis_InstrumentParams::plot_seis_footplane()
  {
    std::vector<SampleCircle> circles;

    add_circle(circles,
               0., 0.,
               seis_radius_meters,
               global_body_max_height);

    return circles;
  }

/***********************************************************************
 * Rotate a set of circles for a given instrument.  The xy position is
 * used to calculate what the rotation should be (based on the tether),
 * and the clock angle IN DEGREES is then added to that.
 * rotateByAngle() is then called to do the rotation.
 ***********************************************************************/

  std::vector<SampleCircle> PigNsytSeis_InstrumentParams::rotateInstrument(
		const std::vector<SampleCircle> &circles,
		const double x, const double y,
		const double clock) const
  {

    double angle;

    angle = seis_rotation(x, y);

    angle += PigDeg2Rad(clock);

    return rotateByAngle(circles, angle);

  }


/***********************************************************************
 * Figure out the SEIS rotation due to the tether
 ***********************************************************************/

  double PigNsytSeis_InstrumentParams::seis_rotation(const double x,
					const double y) const
  {
    return atan2(y - seis_tether_box_y,
                 x - seis_tether_box_x);
  }

/***********************************************************************
 * Read the given instrument shape
 * Replace with reading from files eventually
 ***********************************************************************/

void PigNsytSeis_InstrumentParams::readInstrumentShape(InstrumentShape &shape,
			 RoughnessType type)
{
    readInstrumentParams();

    if (type == INST_BODY) {
        shape.circles = plot_seis_footplane();
    } else if (type == INST_FEET) {
        shape.circles = plot_seis_footpatches();
    } else {
        zvmessage("WARNING!  Invalid SEIS Instrument Type", "");
        zabend();
    }
}


/***********************************************************************
 * Fill up the InstrumentParams structure
 * !!!! This really should be read from a config file.   Constants for now.
 ***********************************************************************/

#include "nsyt_foot_dimensions.h"

void PigNsytSeis_InstrumentParams::readInstrumentParams()
{
    //!!!! This first set was defined in the .cc file rather than the .h
    global_feet_max_height = .01;
    global_body_max_height = .03;
    global_wts_body_max_height = .06;
    num_seis_feet = 3;
    seis_tether_box_x = .280;
    seis_tether_box_y = -.1015;

    // Everything below is in nsyt_foot_dimensions.h

    seis_radius_meters = SEIS_RADIUS_METERS;
    seis_foot_location_radius_meters = SEIS_FOOT_LOCATION_RADIUS_METERS;
    seis_first_patch_offset_phi_degrees = SEIS_FIRST_PATCH_OFFSET_PHI_DEGREES;
    seis_patch_radius_meters = SEIS_PATCH_RADIUS_METERS;
    seis_tether_patch_radius_meters = SEIS_TETHER_PATCH_RADIUS_METERS;
    seis_tether_patch_radius_delta_meters = SEIS_TETHER_PATCH_RADIUS_DELTA_METERS;
    seis_tether_patch_radius_num_increments_start = SEIS_TETHER_PATCH_RADIUS_NUM_INCREMENTS_START;
    seis_tether_patch_radius_num_increments_finish = SEIS_TETHER_PATCH_RADIUS_NUM_INCREMENTS_FINISH;
    seis_tether_offset_phi_degrees = SEIS_TETHER_OFFSET_PHI_DEGREES;

}

