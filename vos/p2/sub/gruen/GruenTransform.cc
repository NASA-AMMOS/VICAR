#include "GruenTransform.h"

#include "GruenTransformAmoeba2.h"
#include "GruenTransformAmoeba4.h"
#include "GruenTransformAmoeba5.h"
#include "GruenTransformAmoeba6.h"
#include "GruenTransformAmoeba8.h"

#include <strings.h>
#include <stdio.h>		// for NULL

////////////////////////////////////////////////////////////////////////
// Define a transform for use by the Gruen correlator.
//
// The general form of this is:
//   x' = ax + by + c + gxy
//   y' = dx + ey + f + hxy
// which implements a generic perspective transform (affine transform plus
// xy terms to map a square to any quadrilateral).
//
// Coefficients are supplied in an array.  The number and order of terms in
// this array varies based on the number of degrees of freedom desired and
// should be considered opaque.  For example, amoeba2 uses just a and d,
// while amoeba(6) uses a-f and amoeba8 uses all 8.  Applications should
// not look directly at e.g. coefs[1] and expect it to mean any particular
// thing.  These modes are implemented by subclasses for efficiency.
//
// The array is continually passed in rather than being kept in the class
// because it allows us to use a single class instantiation with all of
// the myriad calculations needed by the amoeba objective function.
//
// The purpose of this class is speed, not safety, so bounds are not checked...
////////////////////////////////////////////////////////////////////////

static char *names[] = {
	"amoeba2",
	"amoeba4",
	"amoeba5",
	"amoeba6",
	"amoeba8"
	"amoeba2_bicubic",
	"amoeba4_bicubic",
	"amoeba5_bicubic",
	"amoeba6_bicubic",
	"amoeba8_bicubic"
};

////////////////////////////////////////////////////////////////////////
// Factory function using strings.  Note that "amoeba" with no number is
// an alias for amoeba6.
////////////////////////////////////////////////////////////////////////

GruenTransform *GruenTransform::create(const char *type)
{
    for (int i=0; i < sizeof(names)/sizeof(char *); i++) {
	if (strcasecmp(type, names[i]) == 0) {
	    return create((TransformType) (i+1));	// +1 because of None
	}
    }

    // Not found, look for special case

    if (strcasecmp(type, "amoeba") == 0) {
	return create(Amoeba6);
    }

    return NULL;
}

////////////////////////////////////////////////////////////////////////
// Factory function using the enum type
////////////////////////////////////////////////////////////////////////

GruenTransform *GruenTransform::create(TransformType type)
{
    switch (type) {
	case Amoeba2:
	    return new GruenTransformAmoeba2();
	case Amoeba4:
	    return new GruenTransformAmoeba4();
	case Amoeba5:
	    return new GruenTransformAmoeba5();
	case Amoeba6:
	    return new GruenTransformAmoeba6();
	case Amoeba8:
	    return new GruenTransformAmoeba8();
	case Amoeba2_bicubic:
	    return new GruenTransformAmoeba2Bicubic();
	case Amoeba4_bicubic:
	    return new GruenTransformAmoeba4Bicubic();
	case Amoeba5_bicubic:
	    return new GruenTransformAmoeba5Bicubic();
	case Amoeba6_bicubic:
	    return new GruenTransformAmoeba6Bicubic();
	case Amoeba8_bicubic:
	    return new GruenTransformAmoeba8Bicubic();
	default:
	    return NULL;
    }
}

