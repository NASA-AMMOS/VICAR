#include "vicmain_c"
#include "zvproto.h"

void main44()
{
    int unit;
    int count, def;
    int i, s, b, w;

    zveaction("SA", "");
    zvunit(&unit, "INP", 1, NULL);

    zvopen(unit, "OP", "UPDATE", "U_FORMAT", "DOUB", NULL);

    int nl, ns, nb;
    zvget(unit, "NL", &nl, "NS", &ns, "NB", &nb, NULL);

    int loc[2];
    zvp("LOC", loc, &count);

    double dn[20];

    zvparmd("DN", dn, &count, &def, 3, 0);
    if (count < nb) {
	for (i=count; i<nb; i++) {
	    dn[i] = dn[count-1];
	}
    }

    int size;
    zvp("HEIGHT", &size, &count);

    int width;
    zvp("WIDTH", &width, &count);
    if (width % 2 != 1) {
	zvmessage("WIDTH must be an odd number", "");
	zabend();
    }
    int woff = width/2;		// round down so 1->0, 3->1

    // Write the crosshair...

    int line, samp;

    for (b=0; b < nb; b++) {
	for (s=0; s < size; s++) {
	    for (w=-woff; w < width; w++) {

		// Vertical line below
	       line = loc[0] + s;  samp = loc[1] + w;
	       if (line > 0 && line <= nl && samp > 0 && samp <= ns)
	           zvwrit(unit, &dn[b], "band", b+1, "NSAMPS", 1,
			   "line", line, "samp", samp, NULL);

		// Vertical line above
	        line = loc[0] - s;  samp = loc[1] + w;
	        if (line > 0 && line <= nl && samp > 0 && samp <= ns)
	            zvwrit(unit, &dn[b], "band", b+1, "NSAMPS", 1,
			    "line", line, "samp", samp, NULL);

		// Horizontal line right
	        line = loc[0] + w;      samp = loc[1] + s;
	        if (line > 0 && line <= nl && samp > 0 && samp <= ns)
	            zvwrit(unit, &dn[b], "band", b+1, "NSAMPS", 1,
			    "line", line, "samp", samp, NULL);

		// Horizontal line left
	        line = loc[0] + w;      samp = loc[1] - s;
	        if (line > 0 && line <= nl && samp > 0 && samp <= ns)
	            zvwrit(unit, &dn[b], "band", b+1, "NSAMPS", 1,
			    "line", line, "samp", samp, NULL);
	    }
	}
    }
    zvclose(unit, NULL);
}

