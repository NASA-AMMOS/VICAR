#include "marsi_wts_instrument.h"
#include "marsi_utils.h"

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

  void PigNsytWTS_InstrumentParams::plot_inst_feet(
		std::vector<SampleCircle> &circles) const
  {
    // First foot.
    {
      const auto foot_phi = radians(wts_first_patch_offset_phi_degrees);

      add_circle(
        circles,
        wts_foot_location_radius_meters,
        foot_phi,
        wts_patch_radius_meters,
        global_feet_max_height);
    }

    // Second foot.
    {
      const auto foot_phi = radians(wts_second_patch_offset_phi_degrees);

      add_circle(
        circles,
        wts_foot_location_radius_meters,
        foot_phi,
        wts_patch_radius_meters,
        global_feet_max_height);
    }

    // Third foot.
    {
      const auto foot_phi = radians(wts_third_patch_offset_phi_degrees);

      add_circle(
        circles,
        wts_foot_location_radius_meters,
        foot_phi,
        wts_patch_radius_meters,
        global_feet_max_height);
    }
  }

/***********************************************************************/

  std::vector<SampleCircle> PigNsytWTS_InstrumentParams::plot_wts_footpatches()
  {
    std::vector<SampleCircle> circles;

    plot_inst_feet(circles);

    return circles;
  }

/***********************************************************************/

  std::vector<SampleCircle> PigNsytWTS_InstrumentParams::plot_wts_skirt()
  {
    std::vector<SampleCircle> circles;

    add_circle(circles,
               0, 0,
               wts_radius_meters,
               global_wts_body_max_height);

    return circles;
  }


/***********************************************************************
 * Rotate a set of circles for a given instrument.  The xy position is
 * used to calculate what the rotation should be (based on the tether),
 * and the clock angle IN DEGREES is then added to that.
 * rotateByAngle() is then called to do the rotation.
 ***********************************************************************/

  std::vector<SampleCircle> PigNsytWTS_InstrumentParams::rotateInstrument(
		const std::vector<SampleCircle> &circles,
		const double x, const double y,
		const double clock) const
  {

    double angle;

    angle = wts_rotation(x, y);

    angle += PigDeg2Rad(clock);

    return rotateByAngle(circles, angle);

  }

/***********************************************************************
 * Get X offset for  WTS instrument.
 ***********************************************************************/

double PigNsytWTS_InstrumentParams::getOffsetX(const double x,
						const double y) const
{
    // atan2(x, y) because the coordinate system is defined as +y up and +x 
    // right.
    double radial_angle = atan2(x, y);

    return -seis_to_wts_offsets[0] * std::sin(radial_angle)
           - seis_to_wts_offsets[1] * std::cos(radial_angle);
}

/***********************************************************************
 * Get X offset for  WTS instrument.
 ***********************************************************************/

double PigNsytWTS_InstrumentParams::getOffsetY(const double x,
						const double y) const
{
    // atan2(x, y) because the coordinate system is defined as +y up and +x 
    // right.
    double radial_angle = atan2(x, y);

    return -seis_to_wts_offsets[0] * std::cos(radial_angle)
           + seis_to_wts_offsets[1] *std::sin(radial_angle);
}

/***********************************************************************
 * Apply translated offsets to WTS instrument. The radial and cross-radial
 * offsets are converted into X and Y offsets using getOffsetX and 
 * getOffsetY functions.
 ***********************************************************************/

  std::vector<SampleCircle> PigNsytWTS_InstrumentParams::applyOffsets(
                const std::vector<SampleCircle> &circles,
                const double offset_x, const double offset_y) const
  {
    std::vector<SampleCircle> new_circles;

    // iterator through the circles and apply offsets
    FOR_ITERATOR(it, circles)
    {
        auto circle = *it;

        circle.x = it->x + offset_x;
        circle.y = it->y + offset_y; 
     
        new_circles.push_back(circle); 
    }

    assert(new_circles.size() == circles.size());
    return new_circles;
  }

/***********************************************************************
 * Apply translated offsets to WTS instrument. The radial and cross-radial
 * offsets are converted into X and Y offsets using getOffsetX and 
 * getOffsetY functions.
 ***********************************************************************/

  PigPoint PigNsytWTS_InstrumentParams::applyOffsets(const PigPoint raw_point, 
                                             const double offset_x,
                                             const double offset_y) const
  {
    double x = raw_point.getX();
    double y = raw_point.getY();

    PigPoint new_point(x + offset_x, y + offset_y, raw_point.getZ());

    return new_point;
  }

/***********************************************************************
 * Figure out the WTS rotation due to the tether
 ***********************************************************************/

  double PigNsytWTS_InstrumentParams::wts_rotation(const double x,
						const double y) const
  {
    return atan2(y, x);
  }


/***********************************************************************
 * Read the given instrument shape
 * Replace with reading from files eventually
 ***********************************************************************/

void PigNsytWTS_InstrumentParams::readInstrumentShape(InstrumentShape &shape,
			 RoughnessType type)
{
    readInstrumentParams();

    if (type == INST_BODY) {
        shape.circles = plot_wts_skirt();
    } else if (type == INST_FEET) {
        shape.circles = plot_wts_footpatches();
    } else {
        zvmessage("WARNING!  Invalid WTS Instrument Type", "");
        zabend();
    }
}


/***********************************************************************
 * Fill up the InstrumentParams structure
 * !!!! This really should be read from a config file.   Constants for now.
 ***********************************************************************/

#include "nsyt_foot_dimensions.h"

void PigNsytWTS_InstrumentParams::readInstrumentParams()
{
    //!!!! This first set was defined in the .cc file rather than the .h
    global_feet_max_height = .01;
    global_body_max_height = .03;
    global_wts_body_max_height = .06;
    num_wts_feet = 3;

    // Everything below is in nsyt_foot_dimensions.h

    wts_radius_meters = WTS_RADIUS_METERS;
    wts_foot_location_radius_meters = WTS_FOOT_LOCATION_RADIUS_METERS;
    wts_first_patch_offset_phi_degrees = WTS_FIRST_PATCH_OFFSET_PHI_DEGREES;
    wts_second_patch_offset_phi_degrees = WTS_SECOND_PATCH_OFFSET_PHI_DEGREES;
    wts_third_patch_offset_phi_degrees = WTS_THIRD_PATCH_OFFSET_PHI_DEGREES;
    wts_patch_radius_meters = WTS_PATCH_RADIUS_METERS;

    // Paramaters

    int count, def;
    char msg[256];
    zvparmd("WTS_OFF", seis_to_wts_offsets, &count, &def, 2, 0);
    if (count == 1) {
        // Set cross-radial offset to 0 if it is unspecified.
        seis_to_wts_offsets[1] = 0;
    }
    sprintf(msg, "WTS offsets: radial = %fm, cross-radial = %fm", 
            seis_to_wts_offsets[0], seis_to_wts_offsets[1]);
    zvmessage(msg, "");
}

