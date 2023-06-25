/* marsinterp */
#include "vicmain_c"
#include "mars_support.h"
#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigVector.h"
#include "PigCoordSystem.h"
#include "PigCSReference.h"

/* Natgrid has to be upgraded to latest version and rebuild, until then the finctionality is disabled */
#ifdef USE_NATGRID
extern "C"{ 
#include "ngmath.h"
}
#endif

#include<stdio.h>
#include <stdlib.h>
#include<string.h>
#include<time.h>
#include <iostream>
#include <math.h>

using namespace std;

// buffer sizes in main program
#define MAX_INPUTS 1 
#define MAX_OPEN 1

// Value of 0 indicates there is no upper-bound for input image size.
#define MAX_NS 100000
#define MAX_NL 100000

#define MAX_VALID_VALUE 1.0E30

//Assumes "ibuf", "inp_ns" local variables.
#define IBUF(j,k) ( *(ibuf[(0)] + (j) * inp_ns + (k)) )

//Assumes "fbuf", "inp_ns" local variables.
#define FBUF(i,j) ( *(fbuf + (i) * inp_ns + (j)) )

//Assumes "fbuf", "inp_ns" local variables.
#define OFBUF(i,j) ( *(interp_band + (i) * inp_ns + (j)) )

//Assumes "fbuf", "inp_ns" local variables.
#define AFBUF(i,j) ( *(alpha_band + (i) * inp_ns + (j)) )
////////////////////////////////////////////////////////////////////////
class Edge_Point {
public:
        float   pixel;          // map value (height)
        char    r, g, b;        // color value
        int     x;              // map location
        int     y;
        int     xvec;           // edge normal
        int     yvec;
        Edge_Point      *next;  // list link
};

static Edge_Point *edge_list;
static char *hole_map;          // 1 = hole in height map
static int xpix, ypix;          // map dimensions
/////////////////////////////////////////////////////////////////////////////////////
// Is there a hole at x,y? Assuming y is valid; return false if X invalid
static inline int is_hole(int x, int y)
{
        return x>=0 && x<xpix && hole_map[x+y*xpix];
}
/////////////////////////////////////////////////////////////////////////////////////
// Interpolate one hole at (j,i),
// filling in from edge points facing towards the hole.
// Returns interpolated height value.
static inline float interpolate_hole(int j, int i)
{
      float xval = 0.0f;      // accumulated height
      float xwgt = 0.0f;      // accumulated weighting

      // scan list for relevant edge points
      Edge_Point *tedge;
      for (tedge = edge_list; tedge; tedge = tedge->next) {
              int dx = j - tedge->x;
              int dy = i - tedge->y;

              // check dot product, does edge point towards me?
              if (tedge->xvec * dx + tedge->yvec * dy >= 0) {
                      // yes, accumulate weighted by closeness
                      float dist = 1.0f / (dx*dx + dy*dy);
                      dist *= dist;   // 4th power
                      // ** dist *= dist; // 8th?
                      xwgt += dist;
                      xval += tedge->pixel * dist;
              }
      }

      return xval / xwgt;
}
/////////////////////////////////////////////////////////////////////////////////////
// Interpolate one hole at (j,i) with filter of immediate neighbors.
// Returns interpolated height value.
static inline float filter_hole(int j, int i, float *interp_band, int inp_ns)
{
        float xval = 0.0f;
        int wsum = 0;

        // filter weighting by |dx|+|dy|, 0 to 6
        static const int weight[] = { 49, 36, 25, 16, 9, 4, 1 };

        int di, dj;
        for(di=-3; di<=3; di++) {       // 7x7 weighted
                int k = i+di;
                if (k<0 || k>=ypix)
                        continue;
                for(dj=-3; dj<=3; dj++) {
                        int l = j+dj;
                        if (l<0 || l>=xpix || hole_map[l+k*xpix])
                                //if (l<0 || l>=xpix )
                                continue;
                        // valid neighbor pixel
                        int wt = weight[abs(di)+abs(dj)];
                        wsum += wt;
                        //xval += wt * zmap->get_float(l, k, 1);
                        xval += wt * OFBUF(k, l);
                }
        }
        return xval / wsum;
}
/////////////////////////////////////////////////////////////////////////////////////
// Interpolate float (height) map.
// zmap should be 2- or 3-band float.
// rgbmap can be null to disable color interpolation.
// empty = pixel value indicating a hole.
// alpha = nonzero to add 3rd band alpha/coverage channel
void interpolate_map(float *fbuf, float* interp_band, float* alpha_band, int inp_ns, int inp_nl, float empty, int alpha)
{
        int bands;
        //zmap = inzmap;                  // save local copy

        //zmap->get_res(&xpix, &ypix, &bands);
        //zmap->allocate(xpix, ypix, 3);
        xpix = inp_ns;
        ypix = inp_nl;
        bands = 1;


        printf("bands=%d xres=%d yres=%d\n", bands, xpix, ypix);
        // int xpix1 = xpix - 1; // lala !!! never used
        // int ypix1 = ypix - 1; // lala !!! never used

        edge_list = NULL;               // init list
        int edge_count = 0;             // diagnostic

        int i, j, k, l; // i=row, j=col, k=neighbor row, l=neighbor col

        // build map of holes, initialize filtered map
        hole_map = new char[xpix * ypix];
        memset(hole_map, 0, xpix*ypix);
        char *h = hole_map;
        for(i=0; i<ypix; i++) {
                for(j=0; j<xpix; j++, h++) {
                        //float xval = zmap->get_float(j, i, 0);
                        float xval = FBUF(i, j);
                        //if (xval == empty) {
                        if (xval > empty) {
                                *h = 1;
                                //printf("xval = %f, i = %d, j = %d \n", xval, i, j);
                        }
                        else {
                                //zmap->set_float(xval, j, i, 1);
                                OFBUF(i,j) = xval;
                                //printf("bands=%d xres=%d yres=%d\n", bands, xpix, ypix);
                        }
                }
        }
        // optionally add alpha channel
        if (alpha) {
            h = hole_map;
            for(i=0; i<ypix; i++) {
                for(j=0; j<xpix; j++, h++) {
                // barely covered if hole, else opaque
                if (*h)
                    //zmap->set_float(1.0/255.0, j, i, 2);
                    AFBUF(i,j) = 1.0/255;
                else
                    //zmap->set_float(1.0, j, i, 2);
                    AFBUF(i,j) = 1.0;
                }
            }
            //!!!!! ozp
            int radius = 10;
            for (int row = 0; row < ypix; row++) {
                for (int col=0; col < xpix; col++) {
                    //float tValue = zmap->get_float(col, row, 2);
                    float tValue = AFBUF(row,col);
                    for (int cnt1 = row - radius; cnt1 <= row + radius; cnt1++) {
                            if (cnt1 < 0) continue;
                        for (int cnt2 = col - radius; cnt2 <= col + radius; cnt2++) {
                            if (cnt2 < 0) continue;
                            // float testVal = zmap->get_float(cnt2, cnt1, 2);
                            float testVal = AFBUF(cnt1,cnt2);
                            if (testVal > 0.999) {
                                testVal = (float)(((double)radius - sqrt((cnt2-col)*(cnt2-col)+(cnt1-row)*(cnt1-row))) / (double)radius);
                                if (testVal > tValue) {
                                    // zmap->set_float(testVal, col, row, 2);
                                    AFBUF(row,col) = testVal;
                                    tValue = testVal;
                                }
                            }
                        }
                    }
                }
            }
            // rescale the data
            for (int row = 0; row < ypix; row++) {
                for (int col=0; col < xpix; col++) {
                    // float tValue = zmap->get_float(col, row, 2);
                    //zmap->set_float(tValue*255.0, col, row, 2);
                    float tValue = AFBUF(row,col);
                    AFBUF(row,col) = tValue*255.0;
                }
            }
        } // alpha channel

        // build list of edge points with normal vectors
        h = hole_map;
        for(i=0; i<ypix; i++) {
                for(j=0; j<xpix; j++, h++) {
                        if (*h)                 // hole, skip it
                                continue;

                        // check for edginess, accumulate normal
                        int xwgt = 0;
                        int xvec = 0;
                        int yvec = 0;
                        for(k=i-1; k<=i+1; k++) {
                                if (k<0 || k>=ypix) continue;
                                for(l=j-1; l<=j+1; l++) {
                                        if (is_hole(l, k)) {
                                                xwgt++;
                                                xvec += l-j;
                                                yvec += k-i;
                                        }
                                }
                        }
                        if(xwgt) {      // must be an edge
                                edge_count++;
                                Edge_Point *tedge = new Edge_Point();
                                tedge->x = j;
                                tedge->y = i;
                                tedge->xvec = xvec;
                                tedge->yvec = yvec;
                                //tedge->pixel = zmap->get_float(j, i, 1);
                                tedge->pixel = FBUF(i, j);
                                tedge->next = edge_list;
                                edge_list = tedge;
                        }
                }
        }
        fprintf(stderr,"Edge count = %d\n", edge_count);
        if (edge_count == 0)
                return;

        // Build interpolated map. To speed things up, do in 3 phases.

        // Phase 1: full interpolation of edge neighbors
        // this step can be really slow, and doesn't add much...
        Edge_Point *tedge;

        /*
         for (tedge = edge_list; tedge; tedge = tedge->next) {
         i = tedge->y;
         j = tedge->x;

         for(k=i-1; k<=i+1; k++) {
         if (k<0 || k>=ypix) continue;
         for(l=j-1; l<=j+1; l++) {
         if (is_hole(l, k)) {
         //zmap->set_float(interpolate_hole(l, k),
         //               l, k, 1);
         OFBUF(i, j) = interpolate_hole(l, k);
         hole_map[l+k*xpix] = 0;
         }
         }
         }
         }
         fprintf(stderr, "Edge neighbors interpolated\n");
         */

        // Phase 2: full interpolation at subresolution
        for(i=0; i<ypix; i+=4) {
                if ((i%100) == 0)
                        fprintf(stderr, "Sub interpolation row %d\n", i);
                if (i & 4)      // alternate even/odd columns
                        j = 0;
                else
                        j = 2;
                int imap = j + i * xpix;
                for (; j<xpix; j += 4, imap += 4) {
                        if (hole_map[imap]) {
                                //zmap->set_float(interpolate_hole(j,i), j, i, 1);
                                OFBUF(i, j) = interpolate_hole(j,i);
                                hole_map[imap] = 0;
                        }
                }
        }
        fprintf(stderr, "Subres interpolation done\n");

        // Phase 3: local filtering of remaining holes
        h = hole_map;
        for(i=0; i<ypix; i++) {
                if ((i%100) == 0)
                        fprintf(stderr, "Filtering row %d\n", i);
                for (j=0; j<xpix; j++, h++) {
                        if (*h) {
                                // need to interpolate
                                //zmap->set_float(filter_hole(j, i), j, i, 1);
                                OFBUF(i,j) = filter_hole(j,i, interp_band, inp_ns);
                        }
                }
        }

        // Free up allocated data
        Edge_Point *tnext;
        for (tedge = edge_list; tedge; tedge = tnext) {
                tnext = tedge->next;
                delete tedge;
        }
        delete [] hole_map;
}
#ifdef USE_NATGRID
/////////////////////////////////////////////////////////////////////////////////////
// Function Name: interpolate_natgrid
// Description: This function uses NatGrid interpolation 3rd party libraries, and
//              Performs natural neighboring-linear interpolation.
// Author:   Lala P.
// Created:  03/28/2014
// Modified: 05/21/2014
/////////////////////////////////////////////////////////////////////////////////////
void interpolate_natgrid(float *fbuf, float *interp_band,int inp_ns, int inp_nl, float empty, int ex )
{
	// hard coded values for x_min, y_min, and step
	float x_min = 0.0;
	float y_min = 0.0;
	float step = 1.0;
        
        // array to hold the input known values
	float x[inp_nl*inp_ns],y[inp_nl*inp_ns],z[inp_nl*inp_ns];

	// array to hold the axis values
	float xo[inp_nl];
	float yo[inp_ns];

	// result of natgrid
	float *out;
	int ier;

	// rest of the variables
	int nl,ns;		// nl = # lines, ns = # Samples
	int index = 0;
	int cHole = 0;		// Counts the # of holes
	float testVal = 0.0;
	clock_t begin, end;
	double time_spent;

	// generate the axis values
	for (int i = 0; i < inp_nl; i++) {
		xo[i] = x_min + ((float)i * step);
	}
	for (int i = 0; i < inp_ns; i++) {
		yo[i] = y_min + ((float)i * step);
	}
		
	// Seperate data values from holes to be used as input values for natgrid
	// iterate the x axis - lines starting from line 1 (top to bottom)
	
	for(int line = 0; line < inp_nl; line++) {
	
	// iterates the y axis - samples (left to right)
		for (int sample = 0 ; sample < inp_ns; sample ++) {
			testVal = FBUF(line, sample);
			if(testVal > empty) {
				// it is a hole we will skip it for now and fill it with interpolated values later
				cHole++;
			}
			else {
				// it is valid data so populate natgrid input data with it 
				nl = (inp_nl - 1) - line;
				ns = sample;
				x[index] = x_min + ((float)nl * step);
				y[index] = y_min + ((float)ns * step);
				z[index] = testVal * (-1.0); //	to swap x and y axis
				index++;
			}
		}
	}
        
	printf(" Number of holes = %d\n",cHole);
        printf(" Number of data  = %d\n", index);
        printf(" Total (nl * ns) = %d\n", (inp_nl * inp_ns));
	
	// natgrid interpolation starts here
	begin = clock();

	// set some control parameters for Natgrid
	c_nnseti("EXT",ex); // 0 = do not do  extrapolation, 1 = do extrapolation
		
	// do your time consuming job here
	out = c_natgrids(index, y, x, z, inp_ns, inp_nl, yo, xo, &ier);
	if (ier != 0) {
		// Natgrid failed see the error report from it here
		printf (" Error return from c_natgrids = %d\n",ier);
	}
	else {
		end = clock();
		time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
		printf(" Natural Neighbor Interpolation has been done in %10.3f (s)\n", time_spent);
	} 
	// get some of the value inserted to the OFBUF
	int outIndex;
	for(int xNat = 0; xNat < inp_ns; xNat++)
		for(int yNat = 0; yNat < inp_nl; yNat++) {
		outIndex = (xNat * inp_nl) +yNat;
		int ll, ss;
		ss = xNat;
		ll = (inp_nl -1) - yNat;
		testVal = FBUF(ll,ss);
	        if((testVal > empty) && (out[outIndex] == 0.0)) {
			OFBUF(ll,ss) = testVal; // holes that Natgrid did not interpolate either.
		}
		else {
			OFBUF(ll,ss) = out[outIndex]*(-1.0);
		}
	}
/*
// Not required for natgrid yet done for test purposes.
// Create the PLY file to mesh it and see the terran
	FILE *ply;
	
	ply = fopen("nat.ply", "w");
	fprintf(ply,"ply\n");
	fprintf(ply,"format ascii 1.0\n");
	fprintf(ply,"element vertex %d\n",inp_nl*inp_ns);
	fprintf(ply,"property float x\n");
	fprintf(ply,"property float y\n");
	fprintf(ply,"property float z\n");
	fprintf(ply,"end_header\n");
	

	for(int xNat = 0; xNat < inp_ns; xNat++)
		for(int yNat = 0; yNat < inp_nl; yNat++) {
		outIndex = (xNat * inp_nl) +yNat;
		fprintf(ply,"%f	%f %f\n",xo[yNat],yo[xNat],out[outIndex]*(-1.0));
	}
	fclose(ply);
*/
}
#endif
////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////
void main44()
{
    printf("********************************\n");
    int band, line;
    int /*status,*/ count, def, metCount, exCount;
    const size_t msgLen = 256;
    char msg[msgLen];
    int nids;
    char /* mission[64], instrument[64], */ method[64], extrapol[64];
	
    // Inputs
    // PigFileModel *file_models[MAX_INPUTS];
    // PigCameraModel *camera_in[MAX_INPUTS];
    // PigPointingModel *pointing_in[MAX_INPUTS];
    // RadiometryModel *radiometric[MAX_INPUTS];
    // int homogeneous_inputs = TRUE;
    // PigCoordSystem *cs;
    int unit=0;
    int inp_nl=0, inp_ns=0, exFlag;
    // char filename[PIG_MAX_FILENAME_SIZE+1];
	
    // Outputs
    int out_unit[1];
	
    // int do_print = TRUE;                // True if info message should be issued
	
    // User Parameters
	
    float *fbuf=NULL;           // input image band
    float *interp_band;			// interp image
    float *alpha_band;			// alpha image
	
    zvmessage("MARSINTERP version 2019-10-17", "");
	
    zvparmd("BAND",&band,&count,&def,1,0);

    // reads the parameter to determine what interpolation method to use.
    zvp("METHOD", method, &metCount);
    // reads the parameter to decide to (not to) do extrapolation
    zvp("EXTRAPOLATE", extrapol, &exCount);
   
    zvpcnt("INP", &nids);
    if (nids == 1) {
	
	// get Unit id
        zvunit(&unit, "INP", 1, NULL);
		
        // Make sure file is open with U_FORMAT of REAL to match our buffer.
		
	/* status = */ zvopen(unit, "op", "read", "open_act", "sa",
			      "io_act", "sa", "u_format", "real", NULL);
        
        // Get input image dimensions
        zvget(unit, "NL", &inp_nl, "NS", &inp_ns, NULL);
        snprintf(msg, msgLen, "inp_nl = %d, inp_ns = %d\n",inp_nl, inp_ns);
        zvmessage(msg, "");		
    }
    else {
	zvmessage("Interp requires 1 file as input", "");
	zabend();
    }

    fbuf = (float *)malloc(inp_nl * inp_ns * sizeof(float));
    
    // Read in the input file(s)...
    for (int line = 0; line < inp_nl; line++) 
    {
       zvread(unit, fbuf + (line * inp_ns),"BAND", 1, "LINE", line+1, NULL);
    }
	
    // Allocate memory for output interpolation band.  The entire image must
    // be in memory for the subroutine.
	
    interp_band = (float *)malloc(inp_nl * inp_ns * sizeof(float));
    alpha_band = (float *)malloc(inp_nl * inp_ns * sizeof(float));
    if (interp_band == NULL || alpha_band == NULL) {
			snprintf(msg, msgLen, "Unable to allocate memory for Image output");
			zvmessage(msg, "");
			zabend();
    }

    // interpolate the image
    if(!strcmp(method,"LINEAR")){
        printf("Performing linear interpolation.\n");
        printf("********************************\n");
        interpolate_map(fbuf, interp_band, alpha_band, inp_ns, inp_nl, MAX_VALID_VALUE, 1);
   }
   else{
      #ifdef USE_NATGRID
	if(!strcmp(extrapol,"EXTRAPOLATE")){
		printf("Performing natural neighbor interpolation with extrapolation.\n");
       		printf("*************************************************************\n");
		exFlag = 1;
	}
	else{
		printf("Performing natural neighbor interpolation no extrapolation.\n");
	        printf("***********************************************************\n");
		exFlag = 0;
	}
        interpolate_natgrid(fbuf, interp_band, inp_ns, inp_nl, MAX_VALID_VALUE, exFlag); 
      #else
        printf("natural neighbor interpolation is not available on this platform.\n");
        printf("*************************************************************\n");
      #endif
        
    }

    // Open output files. OUT is a single 3-banded file
    zvpcnt("OUT", &count);
    if (count == 1) {
		zvunit(&out_unit[0], "OUT", 1, NULL);
	        zvopen(out_unit[0], "op", "write",
			   "u_ns", inp_ns, "u_nl",inp_nl,
			   "u_nb", 3,
			   "open_act", "sa", "u_org", "bsq",
			   "u_format", "real", "o_format", "real", NULL);
		zvplabel(out_unit[0], 0, 1);
		
		//out_unit[2] = out_unit[1] = out_unit[0];

		// write output label
		//PigLabelModel *labelModel = m->createLabelModel(out_unit[0]);
		// pick the coordinate system to use.
		//labelModel->setUVW(file_models, nids, cs, "UVW_MAP");
  }
    else {
		zvmessage("OUT must have 1 filename", "");
		zabend();
    }	
    // Write out file(s)...
    for (line = 0; line < inp_nl; line++) {	
	zvwrit(out_unit[0], (fbuf + (line * inp_ns)),"BAND", 1, "LINE", line+1, NULL);
	zvwrit(out_unit[0], (interp_band + (line * inp_ns)), "BAND", 2, "LINE", line+1, NULL);
	zvwrit(out_unit[0], (alpha_band + (line * inp_ns)), "BAND", 3, "LINE", line+1, NULL);
    }
    zvclose(out_unit[0], NULL);
    //if (out_unit[1] != out_unit[0])
	//	zvclose(out_unit[1], NULL);
}
