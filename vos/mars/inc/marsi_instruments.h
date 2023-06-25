#ifndef MARS_INSTRUMENTS_H
#define MARS_INSTRUMENTS_H

#include "nanoflann.hpp"
#include "SimpleImage.h"
#include "PigVector.h"

#include <cstdlib>
#include <vector>
#include <utility>


typedef enum {SEIS, HP3, WTS, M20_HELI, M20_PEBBLE_SHIELD} MarsInstrument;
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

template <typename T> struct PointCloud2D
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
	inline T kdtree_distance(const T *p1, const size_t idx_p2,size_t) const
	{
	    const T d0=p1[0]-pts[idx_p2].x;
	    const T d1=p1[1]-pts[idx_p2].y;
	    return d0*d0 + d1*d1;
	}

	// Returns the dim'th component of the idx'th point in the class:
	// Since this is inlined and the "dim" argument is typically an
	// immediate value, the "if/else's" are actually solved at compile time.
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

	// Optional bounding-box computation: return false to default to a
	// standard bbox computation loop.
	// Return true if the BBOX was already computed by the class and
	// returned in "bb" so we can avoid redoing it again. Look at bb.size()
	// to find out the expected dimensionality (eg 2 or 3 for point clouds)

	template <class BBOX> bool kdtree_get_bbox(BBOX& /*bb*/) const
	    { return false; }
};

typedef nanoflann::KDTreeSingleIndexAdaptor<
    nanoflann::L2_Simple_Adaptor<double, PointCloud2D<double> > ,
    PointCloud2D<double>, 2> KDTree;



class PigInstrumentParams
{
    protected:
	double radians(const double degrees) const
	{
	    const double pi = 3.141592653589793;
	    return degrees * pi / 180;
	}

	virtual void add_circle(
		std::vector<SampleCircle> &circles,
		const double radial_coordinate,
		const double angular_coordinate,
		const double radius,
		const double max_height) const;


    public:

	// Factory to construct the correct type

	static PigInstrumentParams *create(const char *inst_type);

	PigInstrumentParams() { }

	// Fill up the InstrumentParams structure
	//!!!! Currently this uses constants or parameters.  Change
	//!!!! to read from a file!!

        virtual void readInstrumentParams() = 0;

        // Read the given instrument shape

        virtual void readInstrumentShape( InstrumentShape &shape,
		RoughnessType type) = 0;

	// True if the instrument is rotationally symmetric for the body
	// (not the legs).  Generally true for all but HP3

	virtual int isBodyRotSymmetric() { return 1; }

	// Rotate a set of circles for a given instrument.  The xy position is
	// used to calculate what the rotation should be (based on the tether),
	// and the clock angle (IN DEGREES) is added to that.
	// rotateByAngle() is then called to do the rotation.

	virtual std::vector<SampleCircle> rotateInstrument(
                const std::vector<SampleCircle> &circles,
                const double x, const double y,
		const double clock) const = 0;

        // Get X offset (used for WTS mostly)

        virtual double getOffsetX(const double x, const double y) const
		{ return 0.0; }

        // Get X offset (used for WTS mostly)

        virtual double getOffsetY(const double x, const double y) const
		{ return 0.0; }
        
        // Apply translated offsets to instrument. The radial and cross-radial
        // offsets are converted into X and Y offsets using getOffsetX and 
        // getOffsetY functions.  For WTS mostly.

        virtual std::vector<SampleCircle> applyOffsets(
                	const std::vector<SampleCircle> &circles,
                	const double offset_x, const double offset_y) const
		{ return circles; }

        virtual PigPoint applyOffsets(const PigPoint raw_point,
                                 const double offset_x,
                                 const double offset_y) const
		{ return raw_point; }

	virtual void plot_inst_feet(std::vector<SampleCircle> &circles) const=0;

	// Rotate a set of circles by the given angle (about the origin)

	virtual std::vector<SampleCircle> rotateByAngle(
                const std::vector<SampleCircle> &circles,
                const double theta_offset) const;

};


PointCloud2D<double> PigI_xyz_to_point_cloud(SimpleImage<double> xyz[3]);
PointCloud2D<double> PigI_xyz_to_point_cloud(SimpleImage<PigPoint> xyz);

// Return index closest coordinate.
// Return -1 if one is not found to be within epsilon.

int KDTree_closest_index(const KDTree &kd_tree,
                    double x,
                    double y,
                    double epsilon);

#endif
