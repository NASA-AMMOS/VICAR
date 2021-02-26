#ifndef GRUENTRANSFORMAMOEBA8_H
#define GRUENTRANSFORMAMOEBA8_H

#include "GruenTransform.h"

////////////////////////////////////////////////////////////////////////
// Defines the 8-dof Amoeba transform for use by the Gruen correlator.
//
// This uses all 8 coefficients.  This implements a generic perspective
// transform with translation, scale, shear, rotation, and trapezoid (or
// perspective) transforms.  This is the most general mode, but may be
// underconstrained for small window sizes.
////////////////////////////////////////////////////////////////////////

class GruenTransformAmoeba8 : public GruenTransform
{
  public:

////////////////////////////////////////////////////////////////////////
// Constructor, really does nothing.
////////////////////////////////////////////////////////////////////////

    GruenTransformAmoeba8() : GruenTransform() { };

////////////////////////////////////////////////////////////////////////
// Compute the X and Y values.  The given array contains the coefficients
// needed for this particular transform.
// A = 0
// B = 1
// C = 2
// D = 3
// E = 4
// F = 5
// G = 6
// H = 7
////////////////////////////////////////////////////////////////////////

    virtual double computeX(double *coefs, double x, double y)
    {
	return coefs[0] * x + coefs[1] * y + coefs[2] + coefs[6] * x * y;
    };
    virtual double computeY(double *coefs, double x, double y)
    {
	return coefs[3] * x + coefs[4] * y + coefs[5] + coefs[7] * x * y;
    };

////////////////////////////////////////////////////////////////////////
// This code is used in the innermost loop of the correlator, so performance
// is critical.	 Therefore the computeX/Y functions, as well as splits of
// those which precompute for a constant Y, are implemented as *macros*
// here so the compiler can inline them, avoiding a virtual function call.
// Makes a big difference... better than 50% improvement in speed!
//
// Usage: if you want to say:
//    newx = xform->computeX(coefs, x, y);
// instead say:
//    GruenTransformComputeX(newx, type, coefs, x, y);
// where type is the GruenTransform type as returned from getType().
////////////////////////////////////////////////////////////////////////

#define GruenTransformComputeXAmoeba8(coefs, x, y)		\
	((coefs)[0] * (x) + (coefs)[1] * (y) + (coefs)[2] + (coefs)[6]*(x)*(y))
#define GruenTransformComputeYAmoeba8(coefs, x, y)		\
	((coefs)[3] * (x) + (coefs)[4] * (y) + (coefs)[5] + (coefs)[7]*(x)*(y))

////////////////////////////////////////////////////////////////////////
// Macros (as above), which precompute some terms for efficiency.  We assume
// Y is constant and X is varying here (as it is in the inner gruen loop).
// The precomputed values are fed back in to the second function.  They are
// additive, so the caller may add additional terms to them.
////////////////////////////////////////////////////////////////////////

#define GruenTransformPreComputeConstYAmoeba8(coefs, pre_x, pre_y, y)	\
	{ pre_x = (coefs)[1] * (y) + (coefs)[2];			\
	  pre_y = (coefs)[4] * (y) + (coefs)[5]; }

#define GruenTransformComputeFromConstYAmoeba8(coefs, res_x, res_y, pre_x, pre_y, x, y) \
        { res_x = (coefs)[0] * (x) + (coefs)[6] * (x) * (y) + (pre_x);	\
	  res_y = (coefs)[3] * (x) + (coefs)[7] * (x) * (y) + (pre_y); }

////////////////////////////////////////////////////////////////////////
// Functions to explicitly return a-h coefficients from the array.
// Those that do not apply to this specific transform are returned as 1 or 0,
// as appropriate for a no-op.
//   x' = ax + by + c + gxy
//   y' = dx + ey + f + hxy
////////////////////////////////////////////////////////////////////////

    virtual double getA(double *coefs) { return coefs[0]; };
    virtual double getB(double *coefs) { return coefs[1]; };
    virtual double getC(double *coefs) { return coefs[2]; };
    virtual double getD(double *coefs) { return coefs[3]; };
    virtual double getE(double *coefs) { return coefs[4]; };
    virtual double getF(double *coefs) { return coefs[5]; };
    virtual double getG(double *coefs) { return coefs[6]; };
    virtual double getH(double *coefs) { return coefs[7]; };

////////////////////////////////////////////////////////////////////////
// Get all the coefs at once
////////////////////////////////////////////////////////////////////////

    virtual void getCoefs(double *coefs,
                double *a, double *b, double *c, double *d,
                double *e, double *f, double *g, double *h)
    {
        *a = coefs[0];
        *b = coefs[1];
        *c = coefs[2];
        *d = coefs[3];
        *e = coefs[4];
        *f = coefs[5];
        *g = coefs[6];
        *h = coefs[7];
    }

////////////////////////////////////////////////////////////////////////
// Set the a-h coefficients into the coefs array.  Those that are not
// used by this particular transform are ignored.
////////////////////////////////////////////////////////////////////////

    virtual void setCoefs(double *coefs,
		double a, double b, double c, double d,
		double e, double f, double g, double h)
    {
	coefs[0] = a;
	coefs[1] = b;
	coefs[2] = c;
	coefs[3] = d;
	coefs[4] = e;
	coefs[5] = f;
	coefs[6] = g;
	coefs[7] = h;
    };

////////////////////////////////////////////////////////////////////////
// Returns the number of coefficients for this transform.
////////////////////////////////////////////////////////////////////////

    virtual int getNumCoefs() { return 8; };

////////////////////////////////////////////////////////////////////////
// Returns the type code for this instantiation.
////////////////////////////////////////////////////////////////////////
 
    virtual TransformType getType() { return Amoeba8; }

};


class GruenTransformAmoeba8Bicubic : public GruenTransformAmoeba8
{
    public:
       virtual TransformType getType() { return Amoeba8_bicubic; }
};
#endif

