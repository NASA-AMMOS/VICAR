////////////////////////////////////////////////////////////////////////
// mars_mosaic_bbox.cc
//
// Computes the bounding box for each input in the mosaic (in terms of output
// mosaic pixels) and writes it to a CSV output file (bbox_filee parameter).
// The corners will always be projected.  Additionally, it will project
// bbox_count pixels along each edge (evenly distributed).
//
// Caller must supply a function to project an input coordinate into the
// the output with the following signature:
//    extern "C" int ProjectFunc(double in_line, double in_samp,
//                              double *out_line, double *out_samp,
//                              int input_number, void *proj_args);
// where "proj_args" is usually a structure of other parameters defined by
// the caller (the function should return 0 for success or non-zero for
// error, such as unprojectable point).
//
// It requires a second function, which returns the width in pixels of
// the given number of degrees (usually 360) at the given point, as well
// as the line number for elevation=0 (for those projections where it makes
// sense) with the following signature:
//    extern "C" int MosaicWidthFunc(double line, double samp, double ndeg,
//		int *out_pix, double *out_line_zeroel,
//		int input_number, void *proj_args);
// where el is the elevation in degrees (needed for sin proj).  For projections
// with no azimuth wrap possible, return 0 for both values.
//
// The following parameters are expected in the PDF:
// PARM BBOX TYPE=STRING COUNT=(0:1) DEFAULT=--
// PARM BBOX_COUNT TYPE=INTEGER COUNT=1 DEFAULT=3
//
// If the bbox parameter is not supplied, we return right away.
//
// Polygons that wrap around in azimuth (possible only for CYL, CYP, SIN),
// will be duplicated with coordinates adjusted for both sides of the wrap.
// In other words, one instance of the polygon for the left side of the
// mosaic, another for the right.
// 
// Images that include the zenith or nadir, have a few extra segments
// added to close the polygon around the zenith or nadir.  See the specific
// comments in the code.
//
// File format is a CSV where the each line is a separate image (image is
// on two lines if it wraps).  First column is the filename, second column
// is a quoted WKT polygon with (samp line) 1-based integer coordinates.
////////////////////////////////////////////////////////////////////////

#include "mars_support.h"

#include "PigFileModel.h"
#include "PigCameraModel.h"

#include "zvproto.h"

extern "C" {
    #include "file_name.h"
}

#include <string.h>
#include <stdio.h>

typedef struct BboxElements {
    double mos_line;
    double mos_samp;
    int pix_360;
    int wrap_amount;
    double line;
    double samp;
    int line_zero_el;
} BboxElements;

static void do_one_bbox_point(BboxElements *bbox, int bbox_idx,
	double line, double samp,
	ProjectFunc func, MosaicWidthFunc wfunc, int index, void *proj_args);

void mars_mosaic_bbox(int nids,
        PigCameraModel *camera_in[], PigFileModel *file_in[], int nlo, int nso,
        ProjectFunc func, MosaicWidthFunc wfunc, void *proj_args)
{
    int i, j;
    int count;

    char bbox_file[PIG_MAX_FILENAME_SIZE];
    zvp("BBOX", bbox_file, &count);
    if (count == 0)
	return;			// nothing to do
    int bbox_count;
    zvp("BBOX_COUNT", &bbox_count, &count);

    zvmessage("Writing bounding box file...", "");
    // Open the CSV file and write the header.

    FILE *bbox_fp = fopen(bbox_file, "w");
    if (bbox_fp == NULL) {
	zvmessage("Unable to open bounding box file", "");
	zabend();
    }

    fprintf(bbox_fp, "filename,WKT_bbox\n");

    // 4 corners plus 4 sides*bbox_count + 3 extra for zenith/nadir
    BboxElements *bbox = new BboxElements[(4*bbox_count + 4) * 3];

    // Process each image - one line per image in output

    for (i = 0; i < nids; i++) {
        int bbox_idx = 0;

        if ((camera_in[i] == NULL) || (file_in[i] == NULL))
            continue;

        double sl, ss, el, es;
        double line, samp;
	double first_line, first_samp, first_mos_line, first_mos_samp;
	first_line = first_samp = first_mos_line = first_mos_samp = -9999;

        file_in[i]->getImageBorders(sl, ss, el, es);


	// Go clockwise around, starting with UL corner

	do_one_bbox_point(bbox, bbox_idx++, sl, ss,
			func, wfunc, i, proj_args);

	// Top side

	double spacing = (es-ss)/(bbox_count+1);

	for (j=1; j < bbox_count+1; j++) {
	    do_one_bbox_point(bbox, bbox_idx++, sl, ss+j*spacing,
				func, wfunc, i, proj_args);
	}

	// UR corner

	do_one_bbox_point(bbox, bbox_idx++, sl, es,
			func, wfunc, i, proj_args);

	// Right side

	spacing = (el-sl)/(bbox_count+1);

	for (j=1; j < bbox_count+1; j++) {
	    do_one_bbox_point(bbox, bbox_idx++, sl+j*spacing, es,
				func, wfunc, i, proj_args);
	}

	// LR corner

	do_one_bbox_point(bbox, bbox_idx++, el, es,
			func, wfunc, i, proj_args);

	// Bottom edge

	spacing = (es-ss)/(bbox_count+1);

	for (j=bbox_count; j >= 1; j--) {
	    do_one_bbox_point(bbox, bbox_idx++, el, ss+j*spacing,
			func, wfunc, i, proj_args);
	}

	// LL corner

	do_one_bbox_point(bbox, bbox_idx++, el, ss,
			func, wfunc, i, proj_args);

	// Left side

	spacing = (el-sl)/(bbox_count+1);

	for (j=bbox_count; j >= 1; j--) {
	    do_one_bbox_point(bbox, bbox_idx++, sl+j*spacing, ss,
			func, wfunc, i, proj_args);
	}

	// Check to see if there's any wrap

	int is_wrap = 0;
	for (int j=0; j < bbox_idx; j++) {
	    if (bbox[j].wrap_amount != 0) {
		is_wrap = bbox[j].wrap_amount;
		break;
	    }
	}

	// If there is wrap, check to see if the wrapped polygon hits the
	// mosaic or not.  We modify the point as we do below before
	// writing it, then include the pix_360 adjustment too to create
	// the second, wrapped poly.  If all the wrapped poly points are
	// off the edge, suppress the wrap.

	int sign = 1;
	if (is_wrap < 0) sign = -1;

	int wrap_on_mosaic = 0;
	for (int j=0; j < bbox_idx; j++) {
	    int wrap_coord = ((bbox[j].mos_samp + 0.5) + 1)
				- bbox[j].wrap_amount + sign*bbox[j].pix_360;
	    if (wrap_coord > 0 && wrap_coord <= nso) {
		wrap_on_mosaic = 1;
	    }
	}

	// If none of the wrap points were on the mosaic, pretend it's no wrap
	if (! wrap_on_mosaic)
	    is_wrap = 0;

	// Now look for nadir/zenith case.  We look for a line segment -
	// after wrap - which extends more than half the wrap size.  If we
	// find one, we assume it's due to a nadir/zenith, and add line
	// segments to fill it out.


	// Write out the line, both standard and wrapped.  Adjust the polys
	// by the wrap amount.  If we're wrapped, add the pix_360 value to
	// make the wrapped poly.

	for (int wrap_idx = 0; wrap_idx <= (is_wrap!=0); wrap_idx++) {

	    fprintf(bbox_fp,"%s,\"POLYGON((", unix_basename((char *)file_in[i]->getFilename()));

	    int point_count = 0;
	    int done_zn_check = 0;

	    for (int j=0; j < bbox_idx; j++) {

	        int phys_line = (int)(bbox[j].mos_line + 0.5) + 1;
	        int phys_samp = (int)(bbox[j].mos_samp + 0.5) + 1;

	        // Adjust the polygon value to eliminate the wrap.

	        phys_samp -= bbox[j].wrap_amount;

	        int wrap = sign*bbox[j].pix_360;
	        if (!is_wrap) wrap = 0;

		// Now add the wrap, only if on the wrap poly

		phys_samp += wrap_idx * wrap;

		if (point_count != 0)
		    fprintf(bbox_fp, ",");
	        fprintf(bbox_fp, "%d %d", phys_samp, phys_line);
		point_count++;

		// Now look for zenith/nadir cases.  We compare to the next
		// point to see if we're more than half the wrap away.  If
		// so, we add points.  We only need to check if there's wrap
		// (although we need it on both wrap polys).  Only do this once.

		if (is_wrap && !done_zn_check) {
		    int next_index = j+1;
		    if (next_index >= bbox_idx)  // last -> first
		        next_index = 0;

	            int next_samp = (int)(bbox[next_index].mos_samp + 0.5) + 1;
	            next_samp -= bbox[next_index].wrap_amount;
		    next_samp += wrap_idx*sign*bbox[next_index].pix_360;

		    if (abs(phys_samp-next_samp) > bbox[next_index].pix_360/2) {

			// We have zenith or nadir.  Figure out which.  We do
			// that by looking at how many points are above or
			// below the line of zero elevation.  Whichever has
			// the most, wins.

			int above_count = 0;
			for (int k=0; k < bbox_idx; k++) {
			    if (bbox[k].mos_line < bbox[k].line_zero_el)
				above_count++;
			}
			int zenith = (above_count >= (bbox_idx/2));

			char msg[1024];
			sprintf(msg, "%s found for input %d bounding box",
				(zenith?"Zenith":"Nadir"), i+1);
			zvmessage(msg, "");

			// Write out the extra points.  First, extend the
			// poly to the "next" point, but unwrapped.  For
			// reasons I don't fully understand, sign seems to
			// not be a factor here.

	        	int next_wrap = bbox[next_index].pix_360;
			int extend_samp = next_samp + next_wrap;
			int extend_line = (int)(bbox[next_index].mos_line
								+0.5) + 1;
			fprintf(bbox_fp, ",%d %d", extend_samp, extend_line);
			point_count++;

			// Now add the top or bottom synthetic points to
			// complete the poly

			int zn_line = 1;
			if (!zenith)
			    zn_line = nlo;
			fprintf(bbox_fp, ",%d %d", extend_samp, zn_line);
			point_count++;

			// For the second extend point, we want the next point

			fprintf(bbox_fp, ",%d %d", next_samp, zn_line);
			point_count++;

		        done_zn_check = 1;
		    }
		}
	    }

	    // Close out the line

	    fprintf(bbox_fp, "))\"\n");
	}
    }

    fclose(bbox_fp);
    delete bbox;
}

//----------------------------------------------------------------------
// Internal routine to calculate and output one point
//
// Each point is compared against the first point.  If any points are
// determined to have wrapped, the entire polygon is considered wrapped.
//
// Wrap status is determined as follows.
// * If we're not cyl/cylper/sin, no wrap.
// * If the points are < 1/4 of the 360-width apart, no wrap.  This helps
//   with some pathological cases with nearly vertical lines.
// * Project the midpoint between the two endpoints.  If the resulting
//   samples are sorted: s1 <= smid <= s2 or s1 >= smid >= s2, then we did
//   not wrap.  If not, we did.
// * If we wrapped, add or subtract the 360-width to the sample value
//   (add if s2 < s1, subtract if s2 > s1).
//
// Points that wrapped are converted to the same wrap-state as the initial
// point by adding or subtracting the 360-width.  The 360-width is the width
// in pixels of a full 360 degrees in azimuth (which is constant for
// cyl/cylper but varies per elevation for sinusoidal).
//----------------------------------------------------------------------

void do_one_bbox_point(BboxElements *bbox, int bbox_idx,
	double line, double samp,
	ProjectFunc func, MosaicWidthFunc wfunc, int index, void *proj_args)
{
    int status;

    // in case of error return
    bbox[bbox_idx].mos_line = -9999;
    bbox[bbox_idx].mos_samp = -9999;
    bbox[bbox_idx].pix_360 = -9999;
    bbox[bbox_idx].wrap_amount = -9999;
    bbox[bbox_idx].line = -9999;
    bbox[bbox_idx].samp = -9999;
    bbox[bbox_idx].line_zero_el = -9999;

    double mos_line, mos_samp;
    status = (*func)(line, samp, &mos_line, &mos_samp, index, proj_args);
    if (status != 0) {
	return;
    }

    // Check wrapround unless it's the first time

    int wrap_value = 0;
    int pix_360 = 0;
    double zeroel = 0;
    status = (*wfunc)(line, samp, 360.0, &pix_360, &zeroel, index, proj_args);
    if (status != 0) {
	return;
    }
    if (bbox_idx != 0) {

	wrap_value = 0;
	if (pix_360 != 0) {		// wraparound is possible

	    // If we're less than 90 deg apart, assume there's no wraparound

	    double first_line = bbox[0].line;
	    double first_samp = bbox[0].samp;
	    double first_mos_line = bbox[0].mos_line;
	    double first_mos_samp = bbox[0].mos_samp;

	    if (fabs(mos_samp - first_mos_samp) >= (pix_360 / 4.0)) {

		// Project the midpoint

		double img_lmid = (line + first_line) / 2.0;
		double img_smid = (samp + first_samp) / 2.0;

		double mos_lmid, mos_smid;

		status = (*func)(img_lmid, img_smid, &mos_lmid, &mos_smid,
				index,proj_args);
		if (status != 0) {
		    return;
		}
		// Check sorting
		if ((first_mos_samp <= mos_smid && mos_smid <= mos_samp) ||
		    (first_mos_samp >= mos_smid && mos_smid >= mos_samp)) {
		    // They're in sorted order, no wrap
		    wrap_value = 0;
		} else {
		    // We wrapped!  Figure out which way to go
		    if (first_mos_samp > mos_samp) {
			wrap_value = - pix_360;
		    } else {
			wrap_value = pix_360;
		    }
		}
	    }
	}
    }

    bbox[bbox_idx].mos_line = mos_line;
    bbox[bbox_idx].mos_samp = mos_samp;
    bbox[bbox_idx].pix_360 = pix_360;
    bbox[bbox_idx].wrap_amount = wrap_value;
    bbox[bbox_idx].line = line;
    bbox[bbox_idx].samp = samp;
    bbox[bbox_idx].line_zero_el = zeroel;

}

