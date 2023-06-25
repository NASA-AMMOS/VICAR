#ifndef NSYT_INSTRUMENTS_H
#define NSYT_INSTRUMENTS_H

#include "nanoflann.hpp"
#include "SimpleImage.h"
#include "PigVector.h"

#include <cstdlib>
#include <vector>
#include <utility>


typedef enum {SEIS, HP3, WTS} InsightInstrument;
typedef enum {INST_BODY=0, INST_FEET=1} RoughnessType;

    struct SampleCircle
    {
        double x;
        double y;
        double radius;

        /** Maximum height this part of the instrument will tolerate. */
        double max_height;
    };

    struct InstrumentShape
    {
	std::vector<SampleCircle> circles;	// List of circles in the shape
    };


    class InstrumentParams
    {
	//!!!! These should all get loaded from a file; constants for now

      private:

	// Constants that were in the .cc file rather than the .h file
	double global_feet_max_height;
	double global_body_max_height;
	double global_wts_body_max_height;
	size_t num_seis_feet;
	size_t num_wts_feet;
	double seis_tether_box_x;
	double seis_tether_box_y;
	double hp3_x1;
	double hp3_y1;
	double hp3_x2;
	double hp3_y2;

	// SEIS

	// Radial and cross-radial offsets from SEIS to WTS
	// The first element is radial offset, and the second is
	// cross-radial offset.
	double seis_to_wts_offsets[2];

	// Radius of SEIS
	double seis_radius_meters;

	// radius at which feet are located
	double seis_foot_location_radius_meters;

	// Offset from x axis to middle of first foot patch.
	// There are three equally spaced feet under SEIS.
	double seis_first_patch_offset_phi_degrees;

	// Radius of foot patch.  This is used by the below
	double seis_patch_radius_meters;

	// Radius of SEIS tether patch.  This is used by the below.
	double seis_tether_patch_radius_meters;

	// We iteratively plot circles along the tether's length.
	// This defines the offset between each circle.
	double seis_tether_patch_radius_delta_meters;

	//!!!! TODO: define in terms of meters instead
	int seis_tether_patch_radius_num_increments_start;
	int seis_tether_patch_radius_num_increments_finish;

	// Offset from x axis to middle of tether's foot patch
	double seis_tether_offset_phi_degrees;

	// WTS

	// Radius of WTS
	double wts_radius_meters;

	// Radius along which WTS foot patches lie
	double wts_foot_location_radius_meters;

	// Offset from x axis to middle of first foot patch
	// There are three non-equally spaced feet under WTS
	double wts_first_patch_offset_phi_degrees;
	double wts_second_patch_offset_phi_degrees;
	double wts_third_patch_offset_phi_degrees;

	// Radius of foot patch
	double wts_patch_radius_meters;

	// HP3

	//!!!! FIXME: These are estimates as the diagram I was given had
	//!!!! no numeric labels for these.

	// Radius of HP3 patch.  This is used by the below.
	double hp3_feet_patch_radius_meters;

	double hp3_wide_feet_foot_location_radius_meters;
	double hp3_narrow_feet_foot_location_radius_meters;
	double hp3_drill_foot_location_radius_meters;
	double hp3_drill_foot_radius_meters;
	double hp3_wide_feet_patch_offset_phi_degrees;
	double hp3_narrow_feet_patch_offset_phi_degrees;

	//!!!! FIXME: Get rid of this.  Derive from the feet parameters

	int hp3_body_delta_degrees;
	int hp3_body_num_increments;

	// We iteratively plot circles along the HP3's length

	// Offset between each circle
	double hp3_patch_radius_delta_meters;

	// Number of iterations we plot in the positive direction
	int hp3_patch_radius_num_positive_increments;

	// Number of iterations we plot in the negative direction
	int hp3_patch_radius_num_negative_increments;

/***********************************************************************/

	// Private functions to fill in the shape

	std::vector<SampleCircle> plot_seis_footpatches();
	std::vector<SampleCircle> plot_wts_footpatches();
  void plot_seis_tether( std::vector<SampleCircle> &circles);
	std::vector<SampleCircle> plot_seis_footplane();
	std::vector<SampleCircle> plot_wts_skirt();
	void plot_hp3_drill(std::vector<SampleCircle> &circles);
	std::vector<SampleCircle> plot_hp3_footpatches();
	std::vector<SampleCircle> plot_hp3_footplanes();
	void plot_hp3_body(std::vector<SampleCircle> &circles);
  void plot_hp3_feet_pair(
    std::vector<SampleCircle> &circles,
    const double offset_phi,
    const double location_radius_meters,
    const double foot_radius) const;

	// Private functions to compute instrument rotation

	double seis_rotation(const double x, const double y) const;
	double wts_rotation(const double x, const double y) const;
	double hp3_rotation(const double x, const double y) const;

/***********************************************************************/

      public:

	// Fill up the InstrumentParams structure
	//!!!! Currently this uses constants or parameters.  Change
	//!!!! to read from a file!!

        void readInstrumentParams();

        // Read the given instrument shape

        void readInstrumentShape( InstrumentShape &shape,
		InsightInstrument inst, RoughnessType type);

	// Rotate a set of circles by the given angle (about the origin)

	std::vector<SampleCircle> rotateByAngle(
                const std::vector<SampleCircle> &circles,
                const double theta_offset) const;

	// Rotate a set of circles for a given instrument.  The xy position is
	// used to calculate what the rotation should be (based on the tether),
	// and the clock angle (IN DEGREES) is added to that.
	// rotateByAngle() is then called to do the rotation.

	std::vector<SampleCircle> rotateInstrument(
                const std::vector<SampleCircle> &circles,
                const double x, const double y,
                const InsightInstrument inst,
		const double clock) const;

        // Get X offset for  WTS instrument.        

        double getOffsetX(const double x, const double y, 
                          const InsightInstrument inst) const;

        // Get X offset for  WTS instrument.       

        double getOffsetY(const double x, const double y,
                          const InsightInstrument inst) const;
        
        // Apply translated offsets to WTS instrument. The radial and cross-radial
        // offsets are converted into X and Y offsets using getOffsetX and 
        // getOffsetY functions.

        std::vector<SampleCircle> applyWTSOffsets(
                const std::vector<SampleCircle> &circles,
                const double offset_x, const double offset_y,
                const InsightInstrument inst) const; 

        PigPoint applyWTSOffsets(const PigPoint raw_point,
                                 const double offset_x,
                                 const double offset_y,
                                 const InsightInstrument inst) const;

      void plot_seis_feet(std::vector<SampleCircle> &circles) const;

      void plot_wts_feet(std::vector<SampleCircle> &circles) const;

      void plot_hp3_wide_feet_pair(std::vector<SampleCircle> &circles) const;
      void plot_hp3_narrow_feet_pair(std::vector<SampleCircle> &circles) const;

    };
  
  std::vector<SampleCircle> rotated(const std::vector<SampleCircle> &circles,
                                    double theta_offset);

  template <typename T>
  struct PointCloud2D
  {
    struct Point
    {
      T x,y;
      int line, samp;
    };

    std::vector<Point> pts;

    // Must return the number of data points
    inline size_t kdtree_get_point_count() const { return pts.size(); }

    // Returns the distance between the vector "p1[0:size-1]" and the data
    // point with index "idx_p2" stored in the class:
    inline T kdtree_distance(const T *p1, const size_t idx_p2,size_t /*size*/) const
    {
      const T d0=p1[0]-pts[idx_p2].x;
      const T d1=p1[1]-pts[idx_p2].y;
      return d0*d0 + d1*d1;
    }

    // Returns the dim'th component of the idx'th point in the class:
    // Since this is inlined and the "dim" argument is typically an immediate
    // value, the "if/else's" are actually solved at compile time.
    inline T kdtree_get_pt(const size_t idx, int dim) const
    {
      if (dim==0) return pts[idx].x;
      else return pts[idx].y;
    }

    // Returns the line/samp coord for the point at the given index
    inline void get_ls(const size_t idx, int &line, int &samp) const
    {
      line = pts[idx].line;
      samp = pts[idx].samp;
    }

    // Optional bounding-box computation: return false to default to a standard
    // bbox computation loop.
    //   Return true if the BBOX was already computed by the class and returned
    //   in "bb" so it can be avoided to redo it again. Look at bb.size() to
    //   find out the expected dimensionality (e.g. 2 or 3 for point clouds)
    template <class BBOX>
    bool kdtree_get_bbox(BBOX& /*bb*/) const { return false; }
  };

  typedef nanoflann::KDTreeSingleIndexAdaptor<
    nanoflann::L2_Simple_Adaptor<double, PointCloud2D<double> > ,
    PointCloud2D<double>,
    2> KDTree;

  PointCloud2D<double> xyz_to_point_cloud(SimpleImage<double> xyz[3]);
  PointCloud2D<double> xyz_to_point_cloud(SimpleImage<PigPoint> xyz);

  /** Return index closest coordinate.
   *
   * Return -1 if one is not found to be within epsilon.
   */
  int closest_index(const KDTree &kd_tree,
                    double x,
                    double y,
                    double epsilon);

  /** Return rotation of SEIS based on tether.
   */
  double seis_rotation(double x, double y);

  /** Return rotation of HP3 based on tether.
   */
  double hp3_rotation(double x, double y);


  /** Output map of rotation values for SEIS.
   */
  void seis_rotation_map(
    double *xyz_input[3],
    size_t num_rows,
    size_t num_cols,
    double *rotation_output);


  /** Output map of rotation values for HP3.
   */
  void hp3_rotation_map(
    double *xyz_input[3],
    size_t num_rows,
    size_t num_cols,
    double *rotation_output);

#endif
