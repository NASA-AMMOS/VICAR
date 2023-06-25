#ifndef MARSI_WTS_INSTRUMENT_H
#define MARSI_WTS_INSTRUMENT_H

#include "marsi_instruments.h"

class PigNsytWTS_InstrumentParams : public PigInstrumentParams
{
	//!!!! These should all get loaded from a file; constants for now

      private:

	// Constants that were in the .cc file rather than the .h file
	double global_feet_max_height;
	double global_body_max_height;
	double global_wts_body_max_height;
	size_t num_wts_feet;

	// Radial and cross-radial offsets from SEIS to WTS
	// The first element is radial offset, and the second is
	// cross-radial offset.
	double seis_to_wts_offsets[2];

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

/***********************************************************************/

	// Private functions to fill in the shape

	std::vector<SampleCircle> plot_wts_footpatches();
	std::vector<SampleCircle> plot_wts_skirt();

	// Private functions to compute instrument rotation

	double wts_rotation(const double x, const double y) const;



/***********************************************************************/

      public:

        PigNsytWTS_InstrumentParams() : PigInstrumentParams() { }

	// Fill up the PigInstrumentParams structure
	//!!!! Currently this uses constants or parameters.  Change
	//!!!! to read from a file!!

        virtual void readInstrumentParams();

        // Read the given instrument shape

        virtual void readInstrumentShape( InstrumentShape &shape,
		RoughnessType type);

        //!!!! Skirt probably IS rotationally symmetric about its own origin...
        //!!!! but how that relates to the offset is currently unclear.
        //!!!! Right now it probably rotates around the SEIS center, but it
        //!!!! probably should rotate around its own center, making clocking
        //!!!! unnecessary.  So for now we disable clocking for this case.


	// Rotate a set of circles for a given instrument.  The xy position is
	// used to calculate what the rotation should be (based on the tether),
	// and the clock angle (IN DEGREES) is added to that.
	// rotateByAngle() is then called to do the rotation.

	virtual std::vector<SampleCircle> rotateInstrument(
                const std::vector<SampleCircle> &circles,
                const double x, const double y,
		const double clock) const;

        // Get X offset for  WTS instrument.        

        virtual double getOffsetX(const double x, const double y) const;

        // Get X offset for  WTS instrument.       

        virtual double getOffsetY(const double x, const double y) const;
        
        // Apply translated offsets to instrument. The radial and cross-radial
        // offsets are converted into X and Y offsets using getOffsetX and 
        // getOffsetY functions.  For WTS mostly.

        virtual std::vector<SampleCircle> applyOffsets(
                const std::vector<SampleCircle> &circles,
                const double offset_x, const double offset_y) const;

        virtual PigPoint applyOffsets(const PigPoint raw_point,
                                 const double offset_x,
                                 const double offset_y) const;

	virtual void plot_inst_feet(std::vector<SampleCircle> &circles) const;

};
  


#endif
