// %M% %I% %E% %U%
// Use MIPL VICAR Run-Time Library to load a VESA library GeoImage object.

#include "image/miplimage.h"
#include "image/datatypes.h"
#include "xvmaininc.h"
#include "zvproto.h"
#include "PigCAHV.h"

// Read in image. File is kept open to support other services
// accessing file header info.
// Returns zero if okay
int MIPLImage::read(const char *fname)
{
	// open the file
	zvunit(&unit, "IMAGE", 1, "u_name", fname, NULL);
	if (zvopen(unit, NULL) != 1)
		return -1;	// failed
		
	// get image parameters from header.
	char fmt[32];
	int xres, yres, bands;
	zvget(unit, "format", fmt, "nl", &yres, "ns", &xres, "nb", &bands,
		NULL);

	// setup image data
	if (!data) {
		switch (fmt[0]) {
		case 'B': // BYTE
			data = new ucharData();		break;
		case 'H':	// HALF
			data = new shortData();		break;
		case 'F':	// FULL
			data = new longData();		break;
		case 'R':	// REAL
			data = new floatData();		break;
		case 'D':	// DOUB
			data = new doubleData();	break;
		default:
			fprintf(stderr, 
				"MIPLImage: Unsupported data format %s\n", 
				fmt);
			cleanup();
			return -2;
		}
	}
	if(!(data->access() & DATA_WRITE)) {
		cleanup();
		return -3;
	}
	if (!data->allocate(xres, yres, bands)) {
		cleanup();
		return -4;
	}
	
	// read image data
	for (int b=0; b<bands; b++) {
		int y;
		switch (fmt[0]) {
		case 'B': {	// BYTE
			uchar *buf = ((ucharData *)data)->get_data(b);
			for (y=1; y<=yres; y++, buf += xres*sizeof(char)) {
				if (zvread(unit, buf, "LINE", y, 
						"BAND", b+1, NULL) != 1) {
					cleanup();
					return -5;
				}
			}
			} break;
		case 'H': {	// HALF
			short *buf = ((shortData *)data)->get_data(b);
			for (y=1; y<=yres; y++, buf += xres*sizeof(short)) {
				if (zvread(unit, buf, "LINE", y, 
						"BAND", b+1, NULL) != 1) {
					cleanup();
					return -5;
				}
			}
			} break;
		case 'F': {	// FULL
			long *buf = ((longData *)data)->get_data(b);
			for (y=1; y<=yres; y++, buf += xres*sizeof(long)) {
				if (zvread(unit, buf, "LINE", y, 
						"BAND", b+1, NULL) != 1) {
					cleanup();
					return -5;
				}
			}
			} break;
		case 'R': {	// REAL
			float *buf = ((floatData *)data)->get_data(b);
			for (y=1; y<=yres; y++, buf += xres*sizeof(float)) {
				if (zvread(unit, buf, "LINE", y, 
						"BAND", b+1, NULL) != 1) {
					cleanup();
					return -5;
				}
			}
			} break;
		case 'D': {	// DOUB
			double *buf = ((doubleData *)data)->get_data(b);
			for (y=1; y<=yres; y++, buf += xres*sizeof(double)) {
				if (zvread(unit, buf, "LINE", y, 
						"BAND", b+1, NULL) != 1) {
					cleanup();
					return -5;
				}
			}
			} break;
		}
	}
	
	// okay, leave unit open for other header accesses	
	
	return 0;
}

// close and release file unit
void MIPLImage::cleanup()
{
	zvclose(unit, "clos_act", "free", NULL);
	unit = -1;
}

// get camera model from file header
int MIPLImage::get_cahv(double C[3], double A[3], double H[3], double V[3])
{
	PigCAHV cmod;
	
	// make sure file was loaded okay
	if (unit < 0)
		return -1;
		
	// Use Planetery Image Geometry library to get CAHV model
	if (cmod.readFromLabel(unit, 1))
		return -2;
	
	// extract components as double arrays
	PigPoint ppc;
	PigVector pva, pvh, pvv;
	cmod.getCurrentCAHV(ppc, pva, pvh, pvv);
	ppc.getXYZ(C);
	pva.getXYZ(A);
	pvh.getXYZ(H);
	pvv.getXYZ(V);
	return 0;
}
