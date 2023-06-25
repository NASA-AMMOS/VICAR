#include <iostream>
using namespace std;
#include "DoubleMatrix.h"

#include "vicmain_c.h"
#include "PigMission.h"
#include "PigFileModel.h"
#include "PigCameraModel.h"
#include "PigLabelModel.h"

#include "mars_support.h"
#include "SimpleImage.h"

#define MAX_INPUTS 1
#define MAX_NS 16384
#define MAX_NL 16384


SimpleImage<double>* nearest( SimpleImage<float> *in_buf, const int& nl_new, const int& ns_new );
SimpleImage<double>* bilinear( SimpleImage<float> *in_buf, const int& nl_new, const int& ns_new );
SimpleImage<double>* bicubic( SimpleImage<float> *in_buf, const int& nl_new, const int& ns_new );


////////////////////////////////////////////////////////////////////////
// MARSRESCALE program is used to rescale an image from the current grid to an arbitrary grid 
// of arbitrary size.  Currently two interpolation methods are supported: bilinear and bicubic spline.
// Bicubic spline method is default. The program requires 1-band input.

void main44()
{
    zvmessage("MARSRESCALE version 2020-05-15", "");

    int nids;
    char mission[64], instrument[64];
    PigFileModel *file_models[MAX_INPUTS+1];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    int homogeneous_inputs = TRUE;
    PigCoordSystem *cs;

    const int MSG_LEN = 256;
    char msg[MSG_LEN];
    int in_unit, out_unit;
    int nl, ns;  // dimensions of original image
    int nb;
    float zoom = 0.0;
    int band;
    int band_count;
    SimpleImage<float> *in_buf;
    SimpleImage<double> *out_image;
    int count, NL_count, NS_count, ZOOM_count;
    int nl_new = 0, ns_new = 0;   // dimensions of the interpolated image

    mars_setup(nids, file_models, camera_in, pointing_in, NULL, cs,
		mission, instrument, homogeneous_inputs,
		MAX_NL, MAX_NS, MAX_INPUTS);
    PigMission *m = PigMission::getMissionObject(mission);

    //int method = zvptst("METHOD");

    //cout << "nids = " << nids << endl;
    snprintf(msg, MSG_LEN, "nids = %d", nids);
    zvmessage(msg, "");

    // *********************************************************
    // MULTI-BAND CODE:  
    // *********************************************************
    nb = file_models[0]->getNB();
    // get parameter overrides if any
    band = 1;         // Band # to process
    zvp("BAND", &band, &count);    // band is band # to process
    band_count = 1;   // The number of bands to process (is either 1 or all (below) )

    if (count == 0) {
        // no input band specified; process all bands
        band_count = nb;   // The number of bands to process (is either 1 (above) or all )
        band = 1;          // First band # to process
        snprintf(msg, MSG_LEN, "Number of bands to be processed is (%d)", band_count);
        zvmessage(msg, "");
    } else {
        // check if input band number is greater than number of bands in input
        if (band > nb) {
            snprintf(msg, MSG_LEN, "Input band (%d) is greater than number of bands in input image. Band set to 1.",
                    band);
            zvmessage(msg, "");
            band = 1;   // Band # to process
        }
    }
    // *********************************************************


    zvp("NL", &nl_new, &NL_count);
    zvp("NS", &ns_new, &NS_count);
    zvp("ZOOM", &zoom, &ZOOM_count);

    //cout << "Input parameters: NL = " << nl_new << ", NS = " << ns_new << ", ZOOM = " << zoom << endl;
    //cout << "NL_count = " << NL_count << ", NS_count = " << NS_count << ", ZOOM_count = " << ZOOM_count << endl << endl;
    snprintf(msg, MSG_LEN, "Input parameters:");
    zvmessage(msg, "");
    snprintf(msg, MSG_LEN, "NL = %d", nl_new);
    zvmessage(msg, "");
    snprintf(msg, MSG_LEN, "NS = %d", ns_new);
    zvmessage(msg, "");
    snprintf(msg, MSG_LEN, "ZOOM = %f", zoom);
    zvmessage(msg, "");

    snprintf(msg, MSG_LEN, "NL_count = %d", NL_count);
    zvmessage(msg, "");
    snprintf(msg, MSG_LEN, "NS_count = %d", NS_count);
    zvmessage(msg, "");
    snprintf(msg, MSG_LEN, "ZOOM_count = %d \n", ZOOM_count);
    zvmessage(msg, "");

    // Open and read in the input

    file_models[0]->closeFile();
    in_unit = file_models[0]->getUnit();

    zveaction("SA", "");
    zvopen(in_unit, "OP", "READ", "U_FORMAT", "REAL", NULL);
    file_models[0]->setFileOpen(TRUE);

    nl = file_models[0]->getNL();
    ns = file_models[0]->getNS();

    //cout << "nl = " << nl << ", ns = " << ns << endl;
    snprintf(msg, MSG_LEN, "nl = %d", nl);
    zvmessage(msg, "");
    snprintf(msg, MSG_LEN, "ns = %d \n", ns);
    zvmessage(msg,"");



    // ZOOM parameter is ignored if (NL,NS) are provided
    if( NL_count == 0 && NS_count == 0 )   // (NL,NS) are NOT provided
    {
        if( ZOOM_count == 0 )   // ZOOM is NOT provided
        {
            zvmessage("Either (NL,NS) or ZOOM parameters should be provided.  Exiting...\n", "");
            exit(1);
        }
        else if( ZOOM_count == 1 )   // ZOOM is provided
        {
            nl_new = zoom*nl;
            ns_new = zoom*ns;
        }
        else
        {
            zvmessage("ZOOM count should be either 0 or 1. Exiting...\n", "");
            exit(1);
        }
    }
    else if( NL_count != 0 && NS_count != 0 )   // (NL,NS) are provided
    {
        zoom = nl_new/nl;
    }
    else
    {
        zvmessage("NL and NS counts should be both 0 or both 1. Exiting...\n", "");
        exit(1);
    }

    //cout << "NL = " << nl_new << ", NS = " << ns_new << ", ZOOM = " << zoom << endl;
    snprintf(msg, MSG_LEN, "Updated parameters:");
    zvmessage(msg, "");
    snprintf(msg, MSG_LEN, "NL = %d", nl_new);
    zvmessage(msg, "");
    snprintf(msg, MSG_LEN, "NS = %d", ns_new);
    zvmessage(msg, "");
    snprintf(msg, MSG_LEN, "ZOOM = %f\n", zoom);
    zvmessage(msg, "");
    

    in_buf = new SimpleImage<float>(nl, ns);
    if (in_buf == NULL) {
	zvmessage("Error allocating input buffer", "");
	zabend();
    }


    // Open the output file.  We use the image file as the primary input.

    zvselpiu(in_unit);
    zvunit(&out_unit, "OUT", 1, NULL);
    zvopen(out_unit, "OP", "WRITE", "U_FORMAT", "DOUB",
			"U_NL", nl_new, "U_NS", ns_new, "U_NB", band_count, NULL);
    zvplabel(out_unit, 0, 1);

    PigLabelModel *labelModel = m->createLabelModel(out_unit);
    //PigCameraModel *camera_model = PigCameraModel::create(file_models[0], NULL);

    camera_in[0]->scaleCamera(zoom, zoom);
     
    labelModel->writeCM(camera_in[0], camera_in[0]->getCoordSystem());
    labelModel->writeProductIds(file_models, nids);  // changed from nids+1


    for( int b = 0; b < band_count; b++ )
    {

        for (int i=0; i < nl; i++) {
	    zvread(in_unit, in_buf->linePtr(i), "line", i+1, "band", b+band, NULL);
        }

        // Initialize output (interpolated) image:
        out_image = new SimpleImage<double>(nl_new, ns_new);

        // Compute scale parameters 
        //double s1 = double(nl_new-1)/double(nl-1);
        //double s2 = double(ns_new-1)/double(ns-1);

        //cout << "s1 = " << s1 << ", s2 = " << s2 << endl;

        // MARSRESCALE program is used to rescale an image from the current grid
        // to an arbitrary grid.  Currently two interpolation methods are supported:
        // bilinear and bicubic spline.  Bicubic spline method is default.

        if( zvptst("BILINEAR") )
        {
            out_image = bilinear( in_buf, nl_new, ns_new );
        }
        else if( zvptst("BICUBIC") )
        {
            out_image = bicubic( in_buf, nl_new, ns_new );
        }
        else if( zvptst("NEAREST") )
        {
            out_image = nearest( in_buf, nl_new, ns_new );
        }

        // Write an image:
        for (int line = 0; line < out_image->getNL(); line++) {
	    zvwrit(out_unit, out_image->linePtr(line), "line", line+1, "band", b+1, NULL);
        }

    } // end of the band loop

    zvclose(in_unit, NULL);
    zvclose(out_unit, NULL);

    zvmessage("Program completed running", "");

}

////////////////////////////////////////////////////////////////////////


SimpleImage<double>* nearest( SimpleImage<float> *in_buf, const int& nl_new, const int& ns_new )
{

    zvmessage("*** NEAREST NEIGHBOOR INTERPOLATION *** \n\n", "");

    const int MSG_LEN = 256;
    char msg[MSG_LEN];

    DoubleVector y(4);      // Vector of function values
    DoubleVector a(4);      // Vector of parameters
    
    int nl = in_buf->getNL();
    int ns = in_buf->getNS();
    long i,j;
    double ii, jj;

    snprintf(msg, MSG_LEN, "nl = %d", nl);
    zvmessage(msg, "");
    snprintf(msg, MSG_LEN, "ns = %d \n", ns);
    zvmessage(msg,"");

    // Declare an output image buffer
    SimpleImage<double> *out_image;

    // Initialize output (interpolated) image:
    out_image = new SimpleImage<double>(nl_new, ns_new);
    if (out_image == NULL) {
	zvmessage("Error allocating output image", "");
	zabend();
    }
    

    for ( i = 0; i < nl_new; i++ )
    {
        for ( j = 0; j < ns_new; j++ )
        {
            ii = ((double(i)+0.5) * double(nl) / double(nl_new)) - 0.5;
            jj = ((double(j)+0.5) * double(ns) / double(ns_new)) - 0.5;

            if( ii >= 0.0 && jj >= 0 && ii < nl-1 && jj < ns-1 )
               out_image->set(i,j, in_buf->get((int)(ii+0.5), (int)(jj+0.5)));
        }
    }
    
    return out_image;
}

////////////////////////////////////////////////////////////////////////

SimpleImage<double>* bilinear( SimpleImage<float> *in_buf, const int& nl_new, const int& ns_new )
{
    // This implementation maps -0.5 -> -0.5 and nl_old-0.5 -> nl_new-0.5 using the transformation:
    //             ii = ((double(i)+0.5) * double(nl_old) / double(nl_new)) - 0.5;
    // which is equivalent to:
    //             i = ((double(ii)+0.5) * double(nl_new) / double(nl_old)) - 0.5;

//    cout << "*** BILINEAR INTERPOLATION *** " << endl << endl;
    zvmessage("*** BILINEAR INTERPOLATION *** \n\n", "");

    const int MSG_LEN = 256;
    char msg[MSG_LEN];

    DoubleMatrix M(4,4);    // Inverse of matrix B
    DoubleVector y(4);      // Vector of function values
    DoubleVector a(4);      // Vector of parameters
    
    // Define the inverted weighting matrix.
    // See Igor Yanovsky, Bilinear and Bicubic Interpolation, Report, 2017 and references in marsrescale.pdf.
    M(1,1) = 1;  M(1,2) =-1;  M(1,3) =-1;  M(1,4) = 1;
    M(2,1) =-1;  M(2,2) = 1;  M(2,3) = 0;  M(2,4) = 0;
    M(3,1) =-1;  M(3,2) = 0;  M(3,3) = 1;  M(3,4) = 0;
    M(4,1) = 1;  M(4,2) = 0;  M(4,3) = 0;  M(4,4) = 0;
    
    cout << M << endl;
    
    int nl = in_buf->getNL();
    int ns = in_buf->getNS();

    // cout << "nl = " << nl << ", ns = " << ns << endl;
    snprintf(msg, MSG_LEN, "nl = %d", nl);
    zvmessage(msg, "");
    snprintf(msg, MSG_LEN, "ns = %d \n", ns);
    zvmessage(msg,"");
 
    double w, h;
    long F00_index_x, F00_index_y, F10_index_x, F10_index_y;
    long F01_index_x, F01_index_y, F11_index_x, F11_index_y;
    long F00, F10, F01, F11;
    
    double x0, x1, y0, y1;
    double temp;

    // Declare an output image buffer
    SimpleImage<double> *out_image;

    // Initialize output (interpolated) image:
    out_image = new SimpleImage<double>(nl_new, ns_new);
    if (out_image == NULL) {
	zvmessage("Error allocating output image", "");
	zabend();
    }
    
    long i,j;

    double ii, jj;

    for ( i = 0; i < nl_new; i++ )
    {
        for ( j = 0; j < ns_new; j++ )
        {
            ii = ((double(i)+0.5) * double(nl) / double(nl_new)) - 0.5;
            jj = ((double(j)+0.5) * double(ns) / double(ns_new)) - 0.5;

            if( ii >= 0.0 && jj >= 0 && ii < nl-1 && jj < ns-1 )
            {

            // Calculate the distance constants

            w = 1 - (ii - floor(ii));
            h = 1 - (jj - floor(jj));

            // Determine indexes of the four neighboring pixels from the original image
            F00_index_x = floor(ii);
            F00_index_y = floor(jj);
            
            F10_index_x = ceil(ii);
            F10_index_y = floor(jj);
            
            F01_index_x = floor(ii);
            F01_index_y = ceil(jj);
            
            F11_index_x = ceil(ii);
            F11_index_y = ceil(jj);
            
            // Calculate the four nearest function values
            F00 = in_buf->get( F00_index_x, F00_index_y );
            F10 = in_buf->get( F10_index_x, F10_index_y );
            F01 = in_buf->get( F01_index_x, F01_index_y );
            F11 = in_buf->get( F11_index_x, F11_index_y );
            
            // Create the y vector
            y(1) = F00;
            y(2) = F10;
            y(3) = F01;
            y(4) = F11;
            
            // Calculate the a vector (ie: the a-values) using simple matrix multiplication

            a = M * y;
            
            // Calculate the value of an image at a pixel
            x0 = 1;
            y0 = 1;
            x1 = 1-w;
            y1 = 1-h;
            
            temp = a(1)*x1*y1 + a(2)*x1*y0 +  a(3)*x0*y1 + a(4)*x0*y0;
            
            out_image->set(i,j,temp);
            }
        }
    }
    
    return out_image;
}

////////////////////////////////////////////////////////////////////////

SimpleImage<double>* bicubic( SimpleImage<float> *in_buf, const int& nl_new, const int& ns_new )
{
    // This implementation maps -0.5 -> -0.5 and nl_old-0.5 -> nl_new-0.5 using the transformation:
    //             ii = ((double(i)+0.5) * double(nl_old) / double(nl_new)) - 0.5;
    // which is equivalent to:
    //             i = ((double(ii)+0.5) * double(nl_new) / double(nl_old)) - 0.5;

//    cout << "*** BICUBIC INTERPOLATION *** " << endl << endl;
    zvmessage("*** BICUBIC INTERPOLATION *** \n\n", "");

    const int MSG_LEN = 256;
    char msg[MSG_LEN];

    DoubleMatrix M(16,16);  // Inverse of matrix B
    DoubleVector z(16);     // Vector of function values and derivative values
    DoubleVector a(16);     // Vector of parameters
    
    // Define the inverted weighting matrix.
    // See Igor Yanovsky, Bilinear and Bicubic Interpolation, Report, 2017 and references in marsrescale.pdf.
    M(1,1) = 4;  M(1,2) =-4;  M(1,3) =-4;  M(1,4) = 4;  M(1,5) = 2;  M(1,6) = 2;  M(1,7) =-2;  M(1,8) =-2;  M(1,9) = 2;  M(1,10) =-2;  M(1,11) = 2;  M(1,12) =-2;  M(1,13) = 1;  M(1,14) = 1;  M(1,15) = 1;  M(1,16) = 1;
    M(2,1) =-6;  M(2,2) = 6;  M(2,3) = 6;  M(2,4) =-6;  M(2,5) =-3;  M(2,6) =-3;  M(2,7) = 3;  M(2,8) = 3;  M(2,9) =-4;  M(2,10) = 4;  M(2,11) =-2;  M(2,12) = 2;  M(2,13) =-2;  M(2,14) =-2;  M(2,15) =-1;  M(2,16) =-1;
    M(3,1) = 0;  M(3,2) = 0;  M(3,3) = 0;  M(3,4) = 0;  M(3,5) = 0;  M(3,6) = 0;  M(3,7) = 0;  M(3,8) = 0;  M(3,9) = 2;  M(3,10) =-2;  M(3,11) = 0;  M(3,12) = 0;  M(3,13) = 1;  M(3,14) = 1;  M(3,15) = 0;  M(3,16) = 0;
    M(4,1) = 2;  M(4,2) =-2;  M(4,3) = 0;  M(4,4) = 0;  M(4,5) = 1;  M(4,6) = 1;  M(4,7) = 0;  M(4,8) = 0;  M(4,9) = 0;  M(4,10) = 0;  M(4,11) = 0;  M(4,12) = 0;  M(4,13) = 0;  M(4,14) = 0;  M(4,15) = 0;  M(4,16) = 0;
    M(5,1) =-6;  M(5,2) = 6;  M(5,3) = 6;  M(5,4) =-6;  M(5,5) =-4;  M(5,6) =-2;  M(5,7) = 4;  M(5,8) = 2;  M(5,9) =-3;  M(5,10) = 3;  M(5,11) =-3;  M(5,12) = 3;  M(5,13) =-2;  M(5,14) =-1;  M(5,15) =-2;  M(5,16) =-1;
    M(6,1) = 9;  M(6,2) =-9;  M(6,3) =-9;  M(6,4) = 9;  M(6,5) = 6;  M(6,6) = 3;  M(6,7) =-6;  M(6,8) =-3;  M(6,9) = 6;  M(6,10) =-6;  M(6,11) = 3;  M(6,12) =-3;  M(6,13) = 4;  M(6,14) = 2;  M(6,15) = 2;  M(6,16) = 1;
    M(7,1) = 0;  M(7,2) = 0;  M(7,3) = 0;  M(7,4) = 0;  M(7,5) = 0;  M(7,6) = 0;  M(7,7) = 0;  M(7,8) = 0;  M(7,9) =-3;  M(7,10) = 3;  M(7,11) = 0;  M(7,12) = 0;  M(7,13) =-2;  M(7,14) =-1;  M(7,15) = 0;  M(7,16) = 0;
    M(8,1) =-3;  M(8,2) = 3;  M(8,3) = 0;  M(8,4) = 0;  M(8,5) =-2;  M(8,6) =-1;  M(8,7) = 0;  M(8,8) = 0;  M(8,9) = 0;  M(8,10) = 0;  M(8,11) = 0;  M(8,12) = 0;  M(8,13) = 0;  M(8,14) = 0;  M(8,15) = 0;  M(8,16) = 0;
    M(9,1) = 0;  M(9,2) = 0;  M(9,3) = 0;  M(9,4) = 0;  M(9,5) = 2;  M(9,6) = 0;  M(9,7) =-2;  M(9,8) = 0;  M(9,9) = 0;  M(9,10) = 0;  M(9,11) = 0;  M(9,12) = 0;  M(9,13) = 1;  M(9,14) = 0;  M(9,15) = 1;  M(9,16) = 0;
    M(10,1)= 0;  M(10,2)= 0;  M(10,3)= 0;  M(10,4)= 0;  M(10,5)=-3;  M(10,6)= 0;  M(10,7)= 3;  M(10,8)= 0;  M(10,9)= 0;  M(10,10)= 0;  M(10,11)= 0;  M(10,12)= 0;  M(10,13)=-2;  M(10,14)= 0;  M(10,15)=-1;  M(10,16)= 0;
    M(11,1)= 0;  M(11,2)= 0;  M(11,3)= 0;  M(11,4)= 0;  M(11,5)= 0;  M(11,6)= 0;  M(11,7)= 0;  M(11,8)= 0;  M(11,9)= 0;  M(11,10)= 0;  M(11,11)= 0;  M(11,12)= 0;  M(11,13)= 1;  M(11,14)= 0;  M(11,15)= 0;  M(11,16)= 0;
    M(12,1)= 0;  M(12,2)= 0;  M(12,3)= 0;  M(12,4)= 0;  M(12,5)= 1;  M(12,6)= 0;  M(12,7)= 0;  M(12,8)= 0;  M(12,9)= 0;  M(12,10)= 0;  M(12,11)= 0;  M(12,12)= 0;  M(12,13)= 0;  M(12,14)= 0;  M(12,15)= 0;  M(12,16)= 0;
    M(13,1)= 2;  M(13,2)= 0;  M(13,3)=-2;  M(13,4)= 0;  M(13,5)= 0;  M(13,6)= 0;  M(13,7)= 0;  M(13,8)= 0;  M(13,9)= 1;  M(13,10)= 0;  M(13,11)= 1;  M(13,12)= 0;  M(13,13)= 0;  M(13,14)= 0;  M(13,15)= 0;  M(13,16)= 0;
    M(14,1)=-3;  M(14,2)= 0;  M(14,3)= 3;  M(14,4)= 0;  M(14,5)= 0;  M(14,6)= 0;  M(14,7)= 0;  M(14,8)= 0;  M(14,9)=-2;  M(14,10)= 0;  M(14,11)=-1;  M(14,12)= 0;  M(14,13)= 0;  M(14,14)= 0;  M(14,15)= 0;  M(14,16)= 0;
    M(15,1)= 0;  M(15,2)= 0;  M(15,3)= 0;  M(15,4)= 0;  M(15,5)= 0;  M(15,6)= 0;  M(15,7)= 0;  M(15,8)= 0;  M(15,9)= 1;  M(15,10)= 0;  M(15,11)= 0;  M(15,12)= 0;  M(15,13)= 0;  M(15,14)= 0;  M(15,15)= 0;  M(15,16)= 0;
    M(16,1) =1;  M(16,2)= 0;  M(16,3)= 0;  M(16,4)= 0;  M(16,5)= 0;  M(16,6)= 0;  M(16,7)= 0;  M(16,8)= 0;  M(16,9)= 0;  M(16,10)= 0;  M(16,11)= 0;  M(16,12)= 0;  M(16,13)= 0;  M(16,14)= 0;  M(16,15)= 0;  M(16,16)= 0;
    
    cout << M << endl;

    int nl = in_buf->getNL();
    int ns = in_buf->getNS();

//    cout << "nl = " << nl << ", ns = " << ns << endl;
    snprintf(msg, MSG_LEN, "nl = %d", nl);
    zvmessage(msg, "");
    snprintf(msg, MSG_LEN, "ns = %d \n", ns);
    zvmessage(msg,"");

    SimpleImage<double> *I, *Ix, *Iy, *Ixy;

    I   = new SimpleImage<double>(nl, ns);
    if (I == NULL) {
	zvmessage("Error allocating I", "");
	zabend();
    }
    Ix  = new SimpleImage<double>(nl, ns);
    if (Ix == NULL) {
	zvmessage("Error allocating Ix", "");
	zabend();
    }
    Iy  = new SimpleImage<double>(nl, ns);
    if (Iy == NULL) {
	zvmessage("Error allocating Iy", "");
	zabend();
    }
    Ixy = new SimpleImage<double>(nl, ns);
    if (Ixy == NULL) {
	zvmessage("Error allocating Ixy", "");
	zabend();
    }

    // Make a copy of the input image
    long i,j;
    for ( i = 0; i < nl; i++ )
    {
        for ( j = 0; j < ns; j++ )
        {
             I->set(i,j,in_buf->get(i,j));
        }
    }

    // Calculate the x-derivatives for each point
    for ( i = 0; i < nl; i++ )
    {
        for ( j = 0; j < ns; j++ )
        {
            if( i == 0 || i == nl-1 )
                Ix->set(i,j,0);
            else
                Ix->set(i,j,0.5 * (I->get(i+1,j) - I->get(i-1,j)));
        }
    }

    // Calculate the y-derivatives for each point
    for ( i = 0; i < nl; i++ )
    {
        for ( j = 0; j < ns; j++ )
        {
            if( j == 0 || j == ns-1 )
                Iy->set(i,j,0);
            else
                Iy->set(i,j,0.5 * (I->get(i,j+1) - I->get(i,j-1)));
        }
    }
    
    // Calculate the xy-derivatives for each point
    for ( i = 0; i < nl; i++ )
    {
        for ( j = 0; j < ns; j++ )
        {
            if( i == 0 || i == nl-1 || j == 0 || j == ns-1 )
                Ixy->set(i,j,0);
            else
                Ixy->set(i,j,0.25 * (I->get(i+1,j+1) - I->get(i+1,j-1) - I->get(i-1,j+1) + I->get(i-1,j-1)));
        }
    }


    double w, h;
    long F00_index_x, F00_index_y, F10_index_x, F10_index_y;
    long F01_index_x, F01_index_y, F11_index_x, F11_index_y;
    long F00, F10, F01, F11;
    long Fx00, Fx10, Fx01, Fx11;
    long Fy00, Fy10, Fy01, Fy11;
    long Fxy00, Fxy10, Fxy01, Fxy11;
    
    double x0, x1, x2, x3, y0, y1, y2, y3;
    double temp1, temp2, temp3, temp4, temp;
    
    // Declare an output image buffer
    SimpleImage<double> *out_image;

    // Initialize output (interpolated) image:
    out_image = new SimpleImage<double>(nl_new, ns_new);
    if (out_image == NULL) {
	zvmessage("Error allocating output image", "");
	zabend();
    }
    
    double ii, jj;

    for ( i = 0; i < nl_new; i++ )
    {
        for ( j = 0; j < ns_new; j++ )
        {
            ii = ((double(i)+0.5) * double(nl) / double(nl_new)) - 0.5;
            jj = ((double(j)+0.5) * double(ns) / double(ns_new)) - 0.5;

            if( ii >= 0.0 && jj >= 0 && ii < nl-1 && jj < ns-1 )
            {

            // Calculate the distance constants

            w = 1 - (ii - floor(ii));
            h = 1 - (jj - floor(jj));
            
            // Determine indexes of the four neighboring pixels from the original image
            F00_index_x = floor(ii);
            F00_index_y = floor(jj);
            
            F10_index_x = ceil(ii);
            F10_index_y = floor(jj);
            
            F01_index_x = floor(ii);
            F01_index_y = ceil(jj);
            
            F11_index_x = ceil(ii);
            F11_index_y = ceil(jj);

            /*
            F00_index_x = floor(i/s1);
            F00_index_y = floor(j/s2);
            
            F10_index_x = ceil(i/s1);
            F10_index_y = floor(j/s2);
            
            F01_index_x = floor(i/s1);
            F01_index_y = ceil(j/s2);
            
            F11_index_x = ceil(i/s1);
            F11_index_y = ceil(j/s2);
            */
            
            // Calculate the four nearest function values
            F00 = I->get( F00_index_x, F00_index_y );
            F10 = I->get( F10_index_x, F10_index_y );
            F01 = I->get( F01_index_x, F01_index_y );
            F11 = I->get( F11_index_x, F11_index_y );
            
            // Calculate the four nearest x-derivatives
            Fx00 = Ix->get( F00_index_x, F00_index_y );
            Fx10 = Ix->get( F10_index_x, F10_index_y );
            Fx01 = Ix->get( F01_index_x, F01_index_y );
            Fx11 = Ix->get( F11_index_x, F11_index_y );
            
            // Calculate the four nearest y-derivatives
            Fy00 = Iy->get( F00_index_x, F00_index_y );
            Fy10 = Iy->get( F10_index_x, F10_index_y );
            Fy01 = Iy->get( F01_index_x, F01_index_y );
            Fy11 = Iy->get( F11_index_x, F11_index_y );
            
            // Calculate the four nearest xy-derivatives
            Fxy00 = Ixy->get( F00_index_x, F00_index_y );
            Fxy10 = Ixy->get( F10_index_x, F10_index_y );
            Fxy01 = Ixy->get( F01_index_x, F01_index_y );
            Fxy11 = Ixy->get( F11_index_x, F11_index_y );
            
            // Create the z vector
            z(1) = F00;
            z(2) = F10;
            z(3) = F01;
            z(4) = F11;
            z(5) = Fx00;
            z(6) = Fx10;
            z(7) = Fx01;
            z(8) = Fx11;
            z(9) = Fy00;
            z(10)= Fy10;
            z(11)= Fy01;
            z(12)= Fy11;
            z(13)= Fxy00;
            z(14)= Fxy10;
            z(15)= Fxy01;
            z(16)= Fxy11;
            
            // Calculate the a vector (ie: the a-values) using simple matrix multiplication
            
            a = M * z;
            
            // Calculate the value of an image at a pixel
            x0 = 1;
            y0 = 1;
            x1 = 1-w;
            y1 = 1-h;
            x2 = (1-w)*(1-w);
            y2 = (1-h)*(1-h);
            x3 = (1-w)*(1-w)*(1-w);
            y3 = (1-h)*(1-h)*(1-h);
            
            temp1 =  a(1)*x3*y3 +  a(2)*x3*y2 +  a(3)*x3*y1 +  a(4)*x3*y0;
            temp2 =  a(5)*x2*y3 +  a(6)*x2*y2 +  a(7)*x2*y1 +  a(8)*x2*y0;
            temp3 =  a(9)*x1*y3 + a(10)*x1*y2 + a(11)*x1*y1 + a(12)*x1*y0;
            temp4 = a(13)*x0*y3 + a(14)*x0*y2 + a(15)*x0*y1 + a(16)*x0*y0;
            
            temp = temp1 + temp2 + temp3 + temp4;
            
            out_image->set(i,j,temp);
            }
        }
    }

    return out_image;
}


