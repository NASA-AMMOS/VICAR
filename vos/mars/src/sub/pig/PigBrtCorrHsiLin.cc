////////////////////////////////////////////////////////////////////////
// PigBrtCorrHsiLin
//
// Implementation of "HsiLin" brightness correction model.  This is similar
// to Linear in that it applies a constant multiplicative and additive factor
// to the entire image (i.e. i' = i * a + b).  However, this factor is applied
// to the Intensity part of HSI space.  The image is converted to HSI, the
// correction is applied, and then it is converted back to RGB space.
//
// Unfortunately we have to re-read the image because we need all bands and
// the infrastructure doesn't support that.  Which means we also have to
// apply rad correction.
//
////////////////////////////////////////////////////////////////////////

#include <string.h>
#include "PigBrtCorrHsiLin.h"
#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "zvproto.h"

////////////////////////////////////////////////////////////////////////
// Constructor.  Returns 0 in *status on success
////////////////////////////////////////////////////////////////////////

PigBrtCorrHsiLin::PigBrtCorrHsiLin() : PigBrtCorrLinear()
{
    _file = NULL;
    _rad = NULL;
}

PigBrtCorrHsiLin::PigBrtCorrHsiLin(PigFileModel *file)
		: PigBrtCorrLinear(file)
{
    _file = file;
    _rad = NULL;
}

PigBrtCorrHsiLin::PigBrtCorrHsiLin(PigFileModel *file, DOMElement *el,
				int *status, RadiometryModel *rad)
		: PigBrtCorrLinear(file, el, status)
{
    _file = file;
    _rad = rad;
}

////////////////////////////////////////////////////////////////////////
// Print the fields of this class.  Subclasses should normally call this,
// then add their own fields.
////////////////////////////////////////////////////////////////////////

void PigBrtCorrHsiLin::print()

{
    char msg[256];

    PigBrtCorrLinear::print();
    printInfo("Applied to HSI colorspace Intensity channel");
}

////////////////////////////////////////////////////////////////////////
// Apply the correction to an image.  max_nl and max_ns represent the
// physical size of the buffer, not the logical extent.  The _sl, _ss,
// _el, and _es member variables define where this physical buffer lies
// in the logical image.  So, an _sl of 2 means that the first line of
// the physical buffer corresponds to the second line of the full-frame
// image.
//
// HSI correction requires 3-band inputs.  If inputs are not 3 bands,
// quietly fall back to Linear correction (the info message is slightly
// different, so the user can tell, but no error is needed).
// Note: band is 0-based
////////////////////////////////////////////////////////////////////////

// Assumes "image_*", and "max_ns" are local variables
#define IMAGE(line, samp) (*((image_int) + (line) * max_ns + (samp)))
#define IMAGEF(line, samp) (*((image_float) + (line) * max_ns + (samp)))

void PigBrtCorrHsiLin::applyCorrectionInternal(void *image,
		int max_nl, int max_ns, int is_float, int band)
{
    int i, j;
    char msg[256];

    if (_file == NULL) {
	printInfo("Unable to apply HsiLin brightness correction with no file model");
	return;
    }

    if (_file->getNB() != 3) {		// Must be 3 bands for HSI
	PigBrtCorrLinear::applyCorrectionInternal(image,
					max_nl, max_ns, is_float, band);
	return;
    }

    sprintf(msg, "Applying HSI correction of mult=%lf add=%lf", _mult, _add);
    printInfo(msg);
    short int *image_int = (short int *)image;
    float *image_float = (float *)image;

    // Since we have to re-read the file anyway, we just read it in as float.
    // That way we don't have to have two versions.  Only when dealing with
    // the user's buffer do we have to worry about it.

    // Create 3 buffers for the RGB conversion

    float *image_r = new float[max_nl * max_ns];
    float *image_g = new float[max_nl * max_ns];
    float *image_b = new float[max_nl * max_ns];
    if (image_r == NULL || image_g == NULL || image_b == NULL) {
	printError("Out of memory in BrtHsiInt");
	delete image_r; delete image_g; delete image_b;
	return;
    }

    // Read the 3 bands.  We can't use mars_read_inputs because it would
    // want to apply brt corr again, an infinite loop.  Plus it's in the
    // mars_ subroutine package not pig.

    _file->closeFile();
    zvopen(_file->getUnit(), "OP", "READ", "U_FORMAT", "REAL",
				"OPEN_ACT", "SA", NULL);
    _file->setFileOpen(TRUE);

    int nl = _file->getNL();
    int ns = _file->getNS();
    for (j=0; j < nl; j++) {
	zvread(_file->getUnit(), image_r + j*max_ns, "BAND", 1, "LINE", j+1,
				NULL);
    }
    for (j=0; j < nl; j++) {
	zvread(_file->getUnit(), image_g + j*max_ns, "BAND", 2, "LINE", j+1,
				NULL);
    }
    for (j=0; j < nl; j++) {
	zvread(_file->getUnit(), image_b + j*max_ns, "BAND", 3, "LINE", j+1,
				NULL);
    }
    _file->closeFile();

    // Apply radiometry if needed

    if (_rad != NULL) {
	_rad->applyCorrection(image_r, nl, ns, 0, FALSE);
	_rad->applyCorrection(image_g, nl, ns, 1, FALSE);
	_rad->applyCorrection(image_b, nl, ns, 2, FALSE);
    }

    double sqrt6 = sqrt(6.0);
    double sqrt2 = sqrt(2.0);
    double sqrt32 = sqrt(3.0/2.0);

    for (j=0; j < (_el - _sl + 1); j++) {
	for (i=0; i < (_es - _ss + 1); i++) {

	    double r = *(image_r + j*max_ns + i);
	    double g = *(image_g + j*max_ns + i);
	    double b = *(image_b + j*max_ns + i);

	    // Make sure all 0's stay all 0, and non-0's stay non-0
	    // (so we don't introduce or lose transparencies)

	    if (r == 0.0 && g == 0.0 && b == 0.0) {
		if (is_float)
		    IMAGEF(j,i) = 0.0;
		else
		    IMAGE(j,i) = 0;
		continue;
	    }

	    // Convert rgb to HSI space.  I'm certain there is a simplification
	    // for this that avoids all the forward/inverse trig functions but
	    // it's not immediately apparent.
	    // This is lifted from the vicar program colort2.
	    // In colort2 the hue and saturation are scaled based on "maxval".
	    // Here we leave them in their "natural" state, which is 0-2pi
	    // radians for hue and 0-1 for sat.  Intensity has the same range
	    // as the inputs.  Note that the PigDeg2Rad(constants) should be
	    // computed by the compiler, so there's no run-time impact.

	    double hue = 0.0;
	    double sat = 0.0;
	    double xint = 0.0;

	    double x = (b+r-2*g) / sqrt6;
	    double y = (b-r) / sqrt2;
	    xint = r;				// max(r,g,b)
	    if (g > xint) xint = g;
	    if (b > xint) xint = b;
	    hue = atan2(y,x);
	    if (hue < 0)
		hue = hue + PigDeg2Rad(360.0);
	    double xhue = PigDeg2Rad(90.0) - hue;
	    while (xhue < PigDeg2Rad(30.0))
		    xhue += PigDeg2Rad(120.0);
	    if (x != 0.0 || y != 0.0) {
		double sinesq = asin(sqrt((x*x+y*y) / (b*b+g*g+r*r)));
		double satlimit = atan(1.0/(sqrt2*sin(xhue)));
		sat = sinesq / satlimit;
		if (sat > 1.0)
		    sat = 1.0;
	    }
	    else
		sat = 0.0;

	    // Now that we have HSI, modify I

	    xint = xint * _mult + _add;

	    // Lots of work for one simple linear adjustment!  Now do the
	    // inverse... convert back to RGB space.  Note that we use xhue
	    // from the above in addition to hue.  Also, the x and y are
	    // not the same in the forward and inverse... so we use xx and yy
	    // below to avoid confusion.  (forward x and y ARE used to avoid
	    // a tangent below).

	    double huefunc = atan(1.0/(sqrt2 * sin(xhue)));
	    double xdist = sin(huefunc * sat);
	    double d = xint * xdist;
	    double dd = d * d;
	    if (fabs(hue-PigDeg2Rad(90.0)) > .000001 &&
		fabs(hue-PigDeg2Rad(270.0)) > .000001) {

		// Simplification to avoid an extra tangent.  hue is just an
		// arctan so we replace it with the ratio.  The tests above
		// take care of divide-by-0 issues.
		// double tang = tan(hue);
	        double tang = y / x;
		huefunc = sqrt(1.0+(tang*tang));
	    }
	    else
		huefunc = 1.0e10;
	    double xx = d / huefunc;
	    double root = dd - xx*xx;
	    if (root < 0.0) root = 0.0;
	    double yy = sqrt(root);
	    if (hue <= PigDeg2Rad(180.0)) yy = -yy;
	    if (hue <= PigDeg2Rad(90.0) || hue > PigDeg2Rad(270.0)) xx = -xx;
	    root = xint*xint - dd;
	    if (root < 0.0) root = 0.0;

	    // provisional values... need to be scaled
	    double red = sqrt32 * yy - xx/sqrt2 + sqrt(root);
	    double green = red + 3.0*xx/sqrt2 - sqrt32*yy;
	    double blue = red - sqrt6*yy;

	    double factor = .000001;
	    if (red > factor) factor = red;
	    if (green > factor) factor = green;
	    if (blue > factor) factor = blue;
	    factor = xint / factor;

	    double dn = 0.0;
	    if (band == 0) dn = factor * red;
	    else if (band == 1) dn = factor * green;
	    else if (band == 2) dn = factor * blue;

	    // Ensure value is non-0 as we stuff it in

	    if (is_float) {
		if (dn == 0.0) dn = 0.000001;	// arbitrary
		IMAGEF(j,i) = dn;
	    }
	    else {
		short int short_dn = (short int)(dn+0.5);
		if (short_dn <= 0) short_dn = 1;
		IMAGE(j,i) = short_dn;
	    }
	}
    }
    delete image_r;
    delete image_g;
    delete image_b;
}

////////////////////////////////////////////////////////////////////////
// Write this piece of the model to the label.  Same as Linear; type is
// handled separately.
////////////////////////////////////////////////////////////////////////

int PigBrtCorrHsiLin::writeToLabel(PigLabelModel *lbl, int index)
{
    return lbl->writeBrtCorrLinearLabel(index, _mult, _add);
}

