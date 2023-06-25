////////////////////////////////////////////////////////////////////////
// Mars roughness support routines
////////////////////////////////////////////////////////////////////////

#include "mars_support.h"

#ifndef MIN
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#endif
#ifndef MAX
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#endif

////////////////////////////////////////////////////////////////////////
// Grow a window around the given point.  Window grows by adding one pixel
// in all four dimensions, creating a "box" (outline) of new pixels.  Window
// stops growing when there are no good points found in the last box, or when
// the window size reaches a maximum.
//
// Caller-supplied function determines whether or not a given pixel is "good"
// according to whatever criteria is desired.  The function can also store
// the pixel in another array, or gather statistics off it, or whatever.
////////////////////////////////////////////////////////////////////////

int mars_grow_window(
    MarsWindowFunc func,	// Function to call on each pt
    void *p,			// arbitrary data passed to func
    int max_window,             // conservative window max
    int x_current, int y_current,  // Center point of window
    int num_cols, int num_rows)	// size of image
{
    int window_size;		// current window size
    int found_a_point;		// found any points in window this iteration?
    int points_found;		// number of points found
    int x0, y0, x1, y1;		// window boundaries
    int xi, yi;			// loop variables for window
    int lim;			// loop limit

    // since we don't know how big a window we need to search in order
    // to find points, start with a small window and keep getting larger
    // until none of the new points in the window meet the user criteria
    // or we exceed the maximum window.

    points_found = 0;
    found_a_point = 0;

    for (window_size = 1;
	    (found_a_point || points_found == 0) && window_size < max_window;
	    window_size++) {

	found_a_point = 0;

        // compute window bounds
        x0 = x_current - window_size;
        x1 = x_current + window_size;
        y0 = y_current - window_size;
        y1 = y_current + window_size;

        // search upper border

        if (y0 >= 0) {
            lim = MIN(x1, num_cols-1);
            yi = y0;
            for (xi = MAX(x0, 0); xi <= lim; xi++) {
                if ((*func)(p, xi, yi)) {
                    found_a_point = 1;
		    points_found++;
                }
            }
        }

        // search lower border

        if (y1 < num_rows) {
            yi = y1;
            lim = MIN(x1, num_cols-1);
            for (xi = MAX(x0, 0); xi <= lim; xi++) {
                if ((*func)(p, xi, yi)) {
                    found_a_point = 1;
		    points_found++;
                }
            }
        }

        // search left border
        // Leave out both ends since they're caught in the top/bottom loops

        if (x0 >= 0) {
            xi = x0;
            lim = MIN(y1, num_rows-1);
            for (yi = MAX(0, y0) + 1; yi <= lim - 1; yi++) {
                if ((*func)(p, xi, yi)) {
                    found_a_point = 1;
		    points_found++;
                }
            }
        }

        // search right border
        // Leave out both ends since they're caught in the top/bottom loops

        if (x1 < num_cols) {
            xi = x1;
            lim = MIN(y1, num_rows-1);
            for (yi = MAX(0, y0) + 1; yi <= lim - 1; yi++) {
                if ((*func)(p, xi, yi)) {
                    found_a_point = 1;
		    points_found++;
                }
            }
        }
    }
    return points_found;
}

