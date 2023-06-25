#include "marsi_instruments.h"
#include "marsi_utils.h"
#include "marsi_seis_instrument.h"
#include "marsi_wts_instrument.h"
#include "marsi_hp3_instrument.h"
#include "marsi_heli_instrument.h"
#include <cfloat>
#include <cmath>
#include <iostream>
#include <cassert>
#include "zvproto.h"


/***********************************************************************/
// Create an instance for the given instrument

PigInstrumentParams *PigInstrumentParams::create(const char *inst_type)
{
    if (strcasecmp(inst_type, "SEIS") == 0) {
	return new PigNsytSeis_InstrumentParams();
    }
    if (strcasecmp(inst_type, "WTS") == 0) {
	return new PigNsytWTS_InstrumentParams();
    }
    if (strcasecmp(inst_type, "HP3") == 0) {
	return new PigNsytHP3_InstrumentParams();
    }
    if (strcasecmp(inst_type, "HELI") == 0) {
	return new PigM20Heli_InstrumentParams();
    }

    char msg[1024];
    sprintf(msg, "Unrecognized instrument type: %s", inst_type);
    zvmessage(msg, "");
    zabend();

    return NULL;
}

/***********************************************************************/
// Add a circle of a given radius at the given radial coordinates
// 

void PigInstrumentParams::add_circle(
			std::vector<SampleCircle> &circles,
			const double radial_coordinate,
			const double angular_coordinate,
			const double radius,
			const double max_height) const
{
  SampleCircle circle;
  circle.x = radial_coordinate * cos(angular_coordinate);
  circle.y = radial_coordinate * sin(angular_coordinate);
  circle.radius = radius;
  circle.max_height = max_height;

printf("ADDING CIRCLE x=%f y=%f rad=%f mh=%f\n", circle.x, circle.y, circle.radius, circle.max_height);		//!!!!
  circles.push_back(circle);
}

/***********************************************************************
 * Rotate a set of circles by a given angle (about the origin)
 ***********************************************************************/

  std::vector<SampleCircle> PigInstrumentParams::rotateByAngle(
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


/***********************************************************************/

  int KDTree_closest_index(const KDTree &kd_tree,
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

  PointCloud2D<double> PigI_xyz_to_point_cloud(SimpleImage<double> xyz[3])
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

  PointCloud2D<double> PigI_xyz_to_point_cloud(SimpleImage<PigPoint> xyz)
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
