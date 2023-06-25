////////////////////////////////////////////////////////////////////////
// mars_write_text.cc
//
// Writes the specified text to the given location in an output image.
// The routine writes directly to the given unit number, which must have
// a U_FORMAT of REAL and be open for update.  Coordinates are 0-based.
//
// Suggested parameters for the PDF (to be read and passed in):
//
// PARM DN TYPE=REAL DEFAULT=4096 COUNT=(1:32)
// PARM ZOOM TYPE=INTEGER DEFAULT=1
//
// The given number of bands are written.  Any 0 values in dn are ignored
// (not written).
////////////////////////////////////////////////////////////////////////

#include "mars_support.h"

#include "zvproto.h"

#include "text.h"

#include <string.h>

#define MAX_OBUF_SIZE 500	// max size of text lines

void mars_write_text(int unit, int nso, int nlo, int nbo, char *string,
		     int x, int y,
		     int zoom, float dn[], WriteTextCenterFlag center)
{
    int i, j, k, status;
    int count;
    float obuf[MAX_OBUF_SIZE];

    // Figure out the size.  ztext() is fixed at 6 cols, 7 rows per char

    int n = strlen(string);

    int text_width = n * zoom * ZTEXT_HORIZONTAL_SIZE;
    int text_height = zoom * ZTEXT_VERTICAL_SIZE;

    int xstart;
    int ystart;

    switch (center) {
	case Center:
	    xstart = x - text_width / 2;
	    ystart = y - text_height / 2;
	    break;
	case Upper_Left:
	    xstart = x;
	    ystart = y;
	    break;
	case Upper_Right:
	    xstart = x - text_width;
	    ystart = y;
	    break;
	case Lower_Left:
	    xstart = x;
	    ystart = y - text_height;
	    break;
	case Lower_Right:
	    xstart = x - text_width;
	    ystart = y - text_height;
	    break;
	case Upper_Center:
	    xstart = x - text_width / 2;
	    ystart = y;
	    break;
	case Lower_Center:
	    xstart = x - text_width / 2;
	    ystart = y - text_height;
	    break;
	case Center_Left:
	    xstart = x;
	    ystart = y - text_height / 2;
	    break;
	case Center_Right:
	    xstart = x - text_width;
	    ystart = y - text_height / 2;
	    break;
	default:
	    zvmessage("Internal error, unknown centering flag in mars_write_text","");
	    zabend();
    }

    if (text_width > MAX_OBUF_SIZE)		// too big for buffer!
	return;

    // If part of the text area goes off the image, slide it a little.

    if (xstart + text_width >= nso)
	xstart = nso - text_width - 1;
    if (xstart < 0)
	xstart = 0;
    if (xstart + text_width >= nso)
	return;			// it just won't fit!
    if (ystart + text_height >= nlo)
	ystart = nlo - text_height - 1;
    if (ystart < 0)
	ystart = 0;
    if (ystart + text_height >= nlo)
	return;			// it just won't fit!

    int save_ystart = ystart;
    for (int b=0; b < nbo; b++) {
	ystart = save_ystart;
	if (dn[b] == 0)		// transparent band
	    continue;

        // initialize buffer
        for (k=0; k < MAX_OBUF_SIZE; k++) 
            obuf[k] = 1.0;

        // Convert text to image, and put it in the output
        // We must go to byte buffer first, then xfer to the float image

        for (j = 0; j < ZTEXT_VERTICAL_SIZE; j++) {
	    unsigned char strout[MAX_OBUF_SIZE];
	    ztext(string, n, j, strout, zoom * ZTEXT_HORIZONTAL_SIZE, 1);
	    for (k=0; k < text_width; k++) {
	        if (strout[k])
		    obuf[k] = dn[b];
	        else
		    obuf[k] = 1;	// not 0 so mosaics won't bleed through
	    }
	    for (k=0; k < zoom; k++) {
	        zvwrit(unit, obuf, "LINE", ystart + 1, "BAND", b+1,
		   "SAMP", xstart + 1, "NSAMPS", text_width, NULL);
	        ystart++;
	    }
	}
    }
}

