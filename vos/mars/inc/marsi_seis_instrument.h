#ifndef MARSI_SEIS_INSTRUMENT_H
#define MARSI_SEIS_INSTRUMENT_H

#include "marsi_instruments.h"

class PigNsytSeis_InstrumentParams : public PigInstrumentParams
{
	//!!!! These should all get loaded from a file; constants for now

      private:

	// Constants that were in the .cc file rather than the .h file
	double global_feet_max_height;
	double global_body_max_height;
	double global_wts_body_max_height;
	size_t num_seis_feet;
	double seis_tether_box_x;
	double seis_tether_box_y;

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


/***********************************************************************/

	// Private functions to fill in the shape

	std::vector<SampleCircle> plot_seis_footpatches();
        void plot_seis_tether( std::vector<SampleCircle> &circles);
	std::vector<SampleCircle> plot_seis_footplane();

	// Private functions to compute instrument rotation

	double seis_rotation(const double x, const double y) const;

	// Return rotation of SEIS based on tether.
	double seis_rotation(double x, double y);


/***********************************************************************/

      public:

	PigNsytSeis_InstrumentParams() : PigInstrumentParams() { }

	// Fill up the InstrumentParams structure
	//!!!! Currently this uses constants or parameters.  Change
	//!!!! to read from a file!!

        virtual void readInstrumentParams();

        // Read the given instrument shape

        virtual void readInstrumentShape( InstrumentShape &shape,
		RoughnessType type);

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
