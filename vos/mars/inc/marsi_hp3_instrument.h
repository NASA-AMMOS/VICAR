#ifndef MARSI_HP3_INSTRUMENT_H
#define MARSI_HP3_INSTRUMENT_H

#include "marsi_instruments.h"

class PigNsytHP3_InstrumentParams : public PigInstrumentParams
{
	//!!!! These should all get loaded from a file; constants for now

    private:

	// Constants that were in the .cc file rather than the .h file
	double global_feet_max_height;
	double global_body_max_height;
	double global_wts_body_max_height;
	double hp3_x1;
	double hp3_y1;
	double hp3_x2;
	double hp3_y2;

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

	void plot_hp3_drill(std::vector<SampleCircle> &circles);
	std::vector<SampleCircle> plot_hp3_footpatches();
	std::vector<SampleCircle> plot_hp3_footplanes();
	void plot_hp3_body(std::vector<SampleCircle> &circles);
	void plot_hp3_narrow_feet_pair(
                std::vector<SampleCircle> &circles) const;
	void plot_hp3_wide_feet_pair(
                std::vector<SampleCircle> &circles) const;


        void plot_hp3_feet_pair(
    			std::vector<SampleCircle> &circles,
    			const double offset_phi,
    			const double location_radius_meters,
    			const double foot_radius) const;

	// Private functions to compute instrument rotation

	double hp3_rotation(const double x, const double y) const;


	// Return rotation of HP3 based on tether.
	double hp3_rotation(double x, double y);


	// Output map of rotation values for HP3.
	void hp3_rotation_map(
	    double *xyz_input[3],
	    size_t num_rows,
	    size_t num_cols,
	    double *rotation_output);


/***********************************************************************/

      public:

        PigNsytHP3_InstrumentParams() : PigInstrumentParams() { }

	// Fill up the InstrumentParams structure
	//!!!! Currently this uses constants or parameters.  Change
	//!!!! to read from a file!!

        virtual void readInstrumentParams();

        // Read the given instrument shape

        virtual void readInstrumentShape( InstrumentShape &shape,
		RoughnessType type);

        // HP3 is NOT rotationally symmetric in the body.

        virtual int isBodyRotSymmetric() { return 0; }

	// Rotate a set of circles for a given instrument.  The xy position is
	// used to calculate what the rotation should be (based on the tether),
	// and the clock angle (IN DEGREES) is added to that.
	// rotateByAngle() is then called to do the rotation.

	virtual std::vector<SampleCircle> rotateInstrument(
                const std::vector<SampleCircle> &circles,
                const double x, const double y,
		const double clock) const;

	virtual void plot_inst_feet(std::vector<SampleCircle> &circles) const;

};
  


#endif
