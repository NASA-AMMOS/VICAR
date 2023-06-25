#include "nsyt_instruments.h"
#include "nsyt_utils.h"
#include "PigVector.h"

#include <cfloat>
#include <cmath>
#include <iostream>
#include <cassert>
#include "zvproto.h"


static double radians(const double degrees)
{
  const double pi = 3.141592653589793;
  return degrees * pi / 180;
}

/***********************************************************************/
// Add a circle of a given radius at the given radial coordinates
// 

static void add_circle(
  std::vector<SampleCircle> &circles,
  const double radial_coordinate,
  const double angular_coordinate,
  const double radius,
  const double max_height) 
{
  SampleCircle circle;
  circle.x = radial_coordinate * cos(angular_coordinate);
  circle.y = radial_coordinate * sin(angular_coordinate);
  circle.radius = radius;
  circle.max_height = max_height;

printf("ADDING CIRCLE x=%f y=%f rad=%f mh=%f\n", circle.x, circle.y, circle.radius, circle.max_height);		//!!!!
  circles.push_back(circle);
}

/***********************************************************************/

    // The plot_* routines all create vectors of circles representing the
    // instruments.  They are internal to here and should be accessed
    // only via readInstrumentShape().

/***********************************************************************/

  void InstrumentParams::plot_seis_tether( std::vector<SampleCircle> &circles)
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

  void InstrumentParams::plot_seis_feet(std::vector<SampleCircle> &circles) const
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

  void InstrumentParams::plot_wts_feet(std::vector<SampleCircle> &circles) const
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

  std::vector<SampleCircle> InstrumentParams::plot_seis_footpatches()
  {
    std::vector<SampleCircle> circles;

    plot_seis_feet(circles);
    //!!!! DISABLED TETHER because it would be way outside any image
    //!!!!plot_seis_tether(circles);

    return circles;
  }

/***********************************************************************/

  std::vector<SampleCircle> InstrumentParams::plot_wts_footpatches()
  {
    std::vector<SampleCircle> circles;

    plot_wts_feet(circles);

    return circles;
  }

/***********************************************************************/

  std::vector<SampleCircle> InstrumentParams::plot_seis_footplane()
  {
    std::vector<SampleCircle> circles;

    add_circle(circles,
               0., 0.,
               seis_radius_meters,
               global_body_max_height);

    return circles;
  }

/***********************************************************************/

  std::vector<SampleCircle> InstrumentParams::plot_wts_skirt()
  {
    std::vector<SampleCircle> circles;

    add_circle(circles,
               0, 0,
               wts_radius_meters,
               global_wts_body_max_height);

    return circles;
  }

/***********************************************************************/

  void InstrumentParams::plot_hp3_body(std::vector<SampleCircle> &circles)
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

  void InstrumentParams::plot_hp3_feet_pair(std::vector<SampleCircle> &circles,
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

  void InstrumentParams::plot_hp3_wide_feet_pair(std::vector<SampleCircle> &circles) const
  {
    plot_hp3_feet_pair(circles,
                       radians(hp3_wide_feet_patch_offset_phi_degrees),
                       hp3_wide_feet_foot_location_radius_meters,
                       hp3_feet_patch_radius_meters);
  }

/***********************************************************************/

  void InstrumentParams::plot_hp3_narrow_feet_pair(std::vector<SampleCircle> &circles) const
  {
    plot_hp3_feet_pair(circles,
                      radians(hp3_narrow_feet_patch_offset_phi_degrees),
                      hp3_narrow_feet_foot_location_radius_meters,
                      hp3_feet_patch_radius_meters);
  }

/***********************************************************************/

  void InstrumentParams::plot_hp3_drill(std::vector<SampleCircle> &circles)
  {
    add_circle(
      circles,
      hp3_drill_foot_location_radius_meters, 0.,
      hp3_drill_foot_radius_meters,
      global_feet_max_height);
  }

/***********************************************************************/

  std::vector<SampleCircle> InstrumentParams::plot_hp3_footpatches()
  {
    std::vector<SampleCircle> circles;

    plot_hp3_narrow_feet_pair(circles);
    plot_hp3_wide_feet_pair(circles);
    plot_hp3_drill(circles);

    return circles;
  }

/***********************************************************************/

  std::vector<SampleCircle> InstrumentParams::plot_hp3_footplanes()
  {
    std::vector<SampleCircle> circles;

    plot_hp3_body(circles);

    return circles;
  }

/***********************************************************************
 * Rotate a set of circles by a given angle (about the origin)
 ***********************************************************************/

  std::vector<SampleCircle> InstrumentParams::rotateByAngle(
		const std::vector<SampleCircle> &circles,
                const double theta_offset) const
  {
    std::vector<SampleCircle> new_circles;
    FOR_ITERATOR(it, circles)
    {
      auto circle = *it;

      circle.x = it->x * std::cos(theta_offset) -
                 it->y * std::sin(theta_offset);

      circle.y = it->x * std::sin(theta_offset) +
                 it->y * std::cos(theta_offset);

      new_circles.push_back(circle);
    }
    assert(new_circles.size() == circles.size());
    return new_circles;
  }


/***********************************************************************
 * Rotate a set of circles for a given instrument.  The xy position is
 * used to calculate what the rotation should be (based on the tether),
 * and the clock angle IN DEGREES is then added to that.
 * rotateByAngle() is then called to do the rotation.
 ***********************************************************************/

  std::vector<SampleCircle> InstrumentParams::rotateInstrument(
		const std::vector<SampleCircle> &circles,
		const double x, const double y,
		const InsightInstrument inst,
		const double clock) const
  {

    double angle;

    if (inst == HP3) {
	angle = hp3_rotation(x, y);
    } else if (inst == WTS) {
	angle = wts_rotation(x, y);
    } else {
	angle = seis_rotation(x, y);
    }

    angle += PigDeg2Rad(clock);

    return rotateByAngle(circles, angle);

  }

/***********************************************************************
 * Get X offset for  WTS instrument.
 ***********************************************************************/

double InstrumentParams::getOffsetX(const double x, const double y, 
                                    const InsightInstrument inst) const
{
    if (inst != WTS) {
        return 0.0;
    }

    // atan2(x, y) because the coordinate system is defined as +y up and +x 
    // right.
    double radial_angle = atan2(x, y);

    return -seis_to_wts_offsets[0] * std::sin(radial_angle)
           - seis_to_wts_offsets[1] * std::cos(radial_angle);
}

/***********************************************************************
 * Get X offset for  WTS instrument.
 ***********************************************************************/

double InstrumentParams::getOffsetY(const double x, const double y,
                                    const InsightInstrument inst) const
{
    if (inst != WTS) {
        return 0.0;
    }

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

  std::vector<SampleCircle> InstrumentParams::applyWTSOffsets(
                const std::vector<SampleCircle> &circles,
                const double offset_x, const double offset_y,
                const InsightInstrument inst) const
  {

    if (inst != WTS) {
        return circles;
    }

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

  PigPoint InstrumentParams::applyWTSOffsets(const PigPoint raw_point, 
                                             const double offset_x,
                                             const double offset_y,
                                             const InsightInstrument inst) const
  {
    if (inst != WTS) {
        return raw_point;
    }

    double x = raw_point.getX();
    double y = raw_point.getY();

    PigPoint new_point(x + offset_x, y + offset_y, raw_point.getZ());

    return new_point;
  }

/***********************************************************************
 * Figure out the SEIS rotation due to the tether
 ***********************************************************************/

  double InstrumentParams::seis_rotation(const double x, const double y) const
  {
    return atan2(y - seis_tether_box_y,
                 x - seis_tether_box_x);
  }

/***********************************************************************
 * Figure out the WTS rotation due to the tether
 ***********************************************************************/

  double InstrumentParams::wts_rotation(const double x, const double y) const
  {
    return atan2(y, x);
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

  double InstrumentParams::hp3_rotation(const double x, const double y) const
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

void InstrumentParams::readInstrumentShape(InstrumentShape &shape,
			 InsightInstrument inst, RoughnessType type)
{
    readInstrumentParams();

    if (inst == SEIS) {
	if (type == INST_BODY) {
	    shape.circles = plot_seis_footplane();
	} else if (type == INST_FEET) {
	    shape.circles = plot_seis_footpatches();
	} else {
	    zvmessage("WARNING!  Invalid SEIS Instrument Type", "");
	    zabend();
	}
    } else if (inst == WTS) {
	if (type == INST_BODY) {
	    shape.circles = plot_wts_skirt();
	} else if (type == INST_FEET) {
	    shape.circles = plot_wts_footpatches();
	} else {
	    zvmessage("WARNING!  Invalid WTS Instrument Type", "");
	    zabend();
	}
    } else if (inst == HP3) {
	if (type == INST_BODY) {
	    shape.circles = plot_hp3_footplanes();
	} else if (type == INST_FEET) {
	    shape.circles = plot_hp3_footpatches();
	} else {
	    zvmessage("WARNING!  Invalid HP3 Instrument Type", "");
	    zabend();
	}
    } else {
	zvmessage("WARNING!  Invalid InSight Instrument", "");
	zabend();
    }
}


/***********************************************************************
 * Fill up the InstrumentParams structure
 * !!!! This really should be read from a config file.   Constants for now.
 ***********************************************************************/

#include "nsyt_foot_dimensions.h"

void InstrumentParams::readInstrumentParams()
{
    //!!!! This first set was defined in the .cc file rather than the .h
    global_feet_max_height = .01;
    global_body_max_height = .03;
    global_wts_body_max_height = .06;
    num_seis_feet = 3;
    num_wts_feet = 3;
    seis_tether_box_x = .280;
    seis_tether_box_y = -.1015;
    hp3_x1 = .083;
    hp3_y1 = -.435;
    hp3_x2 = -1.266;
    hp3_y2 = .147;

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

    wts_radius_meters = WTS_RADIUS_METERS;
    wts_foot_location_radius_meters = WTS_FOOT_LOCATION_RADIUS_METERS;
    wts_first_patch_offset_phi_degrees = WTS_FIRST_PATCH_OFFSET_PHI_DEGREES;
    wts_second_patch_offset_phi_degrees = WTS_SECOND_PATCH_OFFSET_PHI_DEGREES;
    wts_third_patch_offset_phi_degrees = WTS_THIRD_PATCH_OFFSET_PHI_DEGREES;
    wts_patch_radius_meters = WTS_PATCH_RADIUS_METERS;

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

/***********************************************************************/

  int closest_index(const KDTree &kd_tree,
                    const double x,
                    const double y,
                    const double epsilon)
  {
    const size_t num_results = 1;
    std::vector<size_t> ret_index(num_results);
    std::vector<double> out_dist_sqr(num_results);
    const double query_point[2] = {x, y};
    kd_tree.knnSearch(query_point, num_results, &ret_index[0], &out_dist_sqr[0]);

    if (out_dist_sqr[0] <= (epsilon * epsilon))
    {
      return ret_index[0];
    }
    else
    {
      return -1;
    }
  }

/***********************************************************************/

  PointCloud2D<double> xyz_to_point_cloud(SimpleImage<double> xyz[3])
  {
    PointCloud2D<double> cloud;
    size_t nl = xyz[0].getNL();
    size_t ns = xyz[0].getNS();

    for (size_t line = 0; line < nl; line++) {
      for (size_t samp = 0; samp < ns; samp++) {
        PointCloud2D<double>::Point point;
        point.x = xyz[0].get(line,samp);
        point.y = xyz[1].get(line,samp);
	point.line = line;
	point.samp = samp;
        cloud.pts.push_back(point);
      }
    }
    return cloud;
  }

/***********************************************************************/

  PointCloud2D<double> xyz_to_point_cloud(SimpleImage<PigPoint> xyz)
  {
    PointCloud2D<double> cloud;
    size_t nl = xyz.getNL();
    size_t ns = xyz.getNS();

    for (size_t line = 0; line < nl; line++) {
      for (size_t samp = 0; samp < ns; samp++) {
        PointCloud2D<double>::Point point;
	PigPoint xyzpt = xyz.get(line,samp);
        point.x = xyzpt.getX();
        point.y = xyzpt.getY();
	point.line = line;
	point.samp = samp;
        cloud.pts.push_back(point);
      }
    }
    return cloud;
  }
