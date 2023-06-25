#include <stdio.h>
#include <string.h>

#include "MiniHeader.h"
#include "lbl_mini_header.h"
#include "return_status.h"

void write_mini_header(LblMiniHeader_typ &idMiniHeader_, MiniHeader *mh);

#include "vicmain_c"

void main44()
{

    int unit;

    zveaction("SA","");

    // Open image file with label

    zvunit(&unit, "INP", 1, NULL);

    zvopen(unit, "OP", "UPDATE", NULL);

    // Find the file containing the mini-header

    char filename[256];
    int count;
    zvp("MINI_FILE", filename, &count);

    int offset;
    zvp("MINI_OFFSET", &offset, &count);

    // Open it

    FILE *f = fopen(filename, "r");
    if (f == NULL) {
	zvmessage("Unable to open mini-header file!", "");
	zabend();
    }
    int status = fseek(f, offset, SEEK_SET);
    if (status != 0) {
	zvmessage("fseek error reading mini-header!", "");
	zabend();
    }

    // Read the mini-header

    unsigned char mini_header[64];

    int n = fread(mini_header, 1, 64, f);
    if (n != 64) {
	zvmessage("Unable to read mini-header!", "");
	zabend();
    }

    fclose(f);

    // Parse the mini-header

    MiniHeader m;
    status = m.parse_mini_header(mini_header, zvptst("ZSTACK"));
    if (status != 1) {
	zvmessage("Parse failure!", "");
	zabend();
    }

    // Write it to the labels

    LblMiniHeader_typ mhlbl;
    memset(&mhlbl, 0, sizeof(mhlbl));

    write_mini_header(mhlbl, &m);

    status = LblMiniHeader(unit, LBL_WRITE, &mhlbl, 1, MINI_HEADER_PARMS_PROPERTY_NAME);
    if (RTN_FAILURE(status)) {
	zvmessage("Label write failure!", "");
	zabend();
    }

    zvclose(unit, NULL);

    zvmessage("SUCCESS!", "");

}




