#ifndef MARSI_HELI_INSTRUMENT_H
#define MARSI_HELI_INSTRUMENT_H

#include "marsi_instruments.h"

class PigM20Heli_InstrumentParams : public PigInstrumentParams
{
	//!!!! These should all get loaded from a file; constants for now

      private:

	double foot_diameter;
	double min_splay_radius;
	double max_splay_radius;
	double splayed_foot_diameter;
	double mid_splay_radius;
	double first_foot_phi_degrees;
	double foot_spacing_degrees;
	int num_splay_circles;
	int num_heli_feet;
	double heli_body_diameter;	// not really body, but between legs


/***********************************************************************/

	// Private functions to fill in the shape

	std::vector<SampleCircle> plot_heli_footpatches();
	std::vector<SampleCircle> plot_heli_footplane();


/***********************************************************************/

      public:

	PigM20Heli_InstrumentParams() : PigInstrumentParams() { }

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
