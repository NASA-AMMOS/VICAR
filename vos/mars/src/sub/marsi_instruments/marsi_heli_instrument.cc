#include "marsi_heli_instrument.h"

#include <cfloat>
#include <cmath>
#include <iostream>
#include <cassert>
#include "zvproto.h"


/***********************************************************************/

void PigM20Heli_InstrumentParams::plot_inst_feet(
		std::vector<SampleCircle> &circles) const
{
    for (size_t foot_index = 0; foot_index < num_heli_feet; foot_index++) {

        double foot_phi_deg =
		first_foot_phi_degrees + foot_index * foot_spacing_degrees;
	if (foot_phi_deg >= 360.)
	    foot_phi_deg -= 360.;

	double foot_phi = radians(foot_phi_deg);

	// This is to get correctly splayed feet.   However, we don't do
	// this any more (see comment in readInstrumentParams).
#if 0
	double splay_spacing = (max_splay_radius - min_splay_radius) /
				(num_splay_circles-1);
	for (int i=0; i < num_splay_circles; i++) {

	    double radius = min_splay_radius + i * splay_spacing;

	    // height (last parameter) is unused

	    add_circle( circles, radius, foot_phi, foot_diameter/2., 0);
	}
#endif
	// Merged-splay feet

	add_circle(circles, mid_splay_radius, foot_phi,
				splayed_foot_diameter/2, 0);
    }
}


/***********************************************************************/

 std::vector<SampleCircle> PigM20Heli_InstrumentParams::plot_heli_footpatches()
{
    std::vector<SampleCircle> circles;

    plot_inst_feet(circles);

    return circles;
}

/***********************************************************************/

std::vector<SampleCircle> PigM20Heli_InstrumentParams::plot_heli_footplane()
{
    std::vector<SampleCircle> circles;

    add_circle(circles, 0., 0., heli_body_diameter/2., 0);

    return circles;
}

/***********************************************************************
 * Rotate a set of circles for a given instrument.  The xy position is
 * used to calculate what the rotation should be (based on the tether),
 * and the clock angle IN DEGREES is then added to that.
 * rotateByAngle() is then called to do the rotation.
 ***********************************************************************/

std::vector<SampleCircle> PigM20Heli_InstrumentParams::rotateInstrument(
		const std::vector<SampleCircle> &circles,
		const double x, const double y,
		const double clock) const
{

    double angle = 0.0;		// no tether

    angle += PigDeg2Rad(clock);

    return rotateByAngle(circles, angle);

}


/***********************************************************************
 * Read the given instrument shape
 * Replace with reading from files eventually
 ***********************************************************************/

void PigM20Heli_InstrumentParams::readInstrumentShape(InstrumentShape &shape,
			 RoughnessType type)
{
    readInstrumentParams();

    if (type == INST_BODY) {
        shape.circles = plot_heli_footplane();
    } else if (type == INST_FEET) {
        shape.circles = plot_heli_footpatches();
    } else {
        zvmessage("WARNING!  Invalid HELI Instrument Type", "");
        zabend();
    }
}


/***********************************************************************
 * Fill up the InstrumentParams structure
 * !!!! This really should be read from a config file.   Constants for now.
 ***********************************************************************/

void PigM20Heli_InstrumentParams::readInstrumentParams()
{

    // These are the real params for the feet

    foot_diameter = 0.050;		// 50 mm
    min_splay_radius = 0.424;
    max_splay_radius = 0.484;		// 60mm splay

    // These are the params we use.  Combine all the splayed circles into
    // one big circle.  This is because there's an implicit assumption in
    // marsi_tilt (at least) that each foot has just one circle.  This should
    // be fixed.  However, since we're going to clock all the way around
    // anyway, it really doesn't matter for the heli use case.

    splayed_foot_diameter = 0.11;	// (.484-.424)+.05
    mid_splay_radius = 0.454;		// (.484+.424)/2

    first_foot_phi_degrees = 45.0;	// make it square instead of diamond
    foot_spacing_degrees = 90.0;
    num_splay_circles = 3;
    num_heli_feet = 4;
    heli_body_diameter = 0.484;		// max splay size (600mm square)

}

