/* mars disparity error analysis */
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <iostream>
using namespace std;

#include "vicmain_c"

#include "mars_support.h"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigVector.h"
#include "RadiometryModel.h"
#include "PigCoordSystem.h"
#include "mat3.h"

#include "return_status.h"

#include "SimpleImage.h"

/* buffer sizes in main program */
#define MAX_INPUTS 3 // 2
#define MAX_NS 2048
#define MAX_NL 65536		/* arbitrary; lines are not buffered */
#define BAD_DELTA -999999

#ifndef MIN
#define MIN(x,y) ((x)<(y) ? (x) : (y))
#endif

#ifndef MAX
#define MAX(x,y) ((x)>(y) ? (x) : (y))
#endif


// disp_unc (disparity uncertainty) and isa (inverse scene activity coefficient) are used interchangeably

SimpleImage<double> compute_disp_unc( const double noise_var, SimpleImage<double>& I, const int nl, const int ns, const int w );

void compute_DispErrors( SimpleImage<double>& isa, const float CR, const int nl, const int ns, const int LS_order, SimpleImage<double>& dl, SimpleImage<double>& ds );

void compute_DispErrors_based_on_CorrelationQuality( SimpleImage<double>& ci, const int nl, const int ns, const int LS_order, SimpleImage<double>& dl, SimpleImage<double>& ds );

void get_DispErrors_from_CompressionRate( const int CR, int& dl, int& ds );

SimpleImage<double> compute_curv( SimpleImage<double>& f, const int nl, const int ns );

void open_inputs(  int nids, PigFileModel *file_models[], int unit[3], int band[3], char *type );
void open_inputs2_allow_different_dimensions( int nids, PigFileModel *file_models[], int unit[2], int band[2], char *type );
void open_inputs1( int nids, PigFileModel *file_models[], int& unit  , int& band  , char *type );

// ################################################################################

void main44()
{
    int i, j;
    int count, def;
    const size_t msgLen = 150;
    char msg[msgLen];

    int nids;
    int inp_unit[2];
    int inp_band[2];
    //int c_nids;
    //int c_unit;
    //int c_band;
    char mission[64], instrument[64];
    char filename[PIG_MAX_FILENAME_SIZE+1];
    int nl, ns;

    // Inputs
	
    SimpleImage<double> I1; //, I2;
    SimpleImage<double> ci;		// correlation coefficient image

	
    // For file models, in addition to creating
    // file model for each input, we also create
    // file models for Disparity Map(s).
    PigFileModel *file_models[MAX_INPUTS+2];
    //PigFileModel *c_file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    int homogeneous_inputs = TRUE;
    PigCoordSystem *cs;
	
    // Outputs

    int out_unit[2];  // units for disparity line and disparity sample error images
    int out_band[2];
	
    int disp_unc_unit;
	
    SimpleImage<double> dli;   // disparity line error
    SimpleImage<double> dsi;   // disparity sample error

    SimpleImage<double> disp_unc;	// inverse scene activity coefficients image
		
    // User Parameters
	
    // Defaults in pdf file may be different from below:
	
    int ls_order = 1;	// Order of Least-Squares fit
    int wsize = 9;	// window size for determining inverse scene activity coefficient

    zvmessage("MARSERRDISP version 2020-02-06", "");
	
    //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
    // Get parameter overrides if any

    zvparmd("LS_ORDER" , &ls_order, &count, &def, 1, 0);
    zvparmd("WSIZE" , &wsize, &count, &def, 1, 0);

	
    double error_params[] = {(double) ls_order, (double) wsize};
    int error_params_cnt = 2;
	
    snprintf(msg, msgLen, "Least Squares order fit: %d", ls_order );
    zvmessage(msg, "");
	
    snprintf(msg, msgLen, "Window size for determining scene activity coefficient: %d", wsize);
    zvmessage(msg, "");

	
    //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    // Get the input file list, and set up initial camera/pointing models
    // for each input.  Although we accept two and only two inputs, mars_setup
    // does lots of other nice things for us.

    mars_setup(nids, file_models, camera_in, pointing_in, NULL, cs,
                mission, instrument, homogeneous_inputs,
                MAX_NL, MAX_NS, MAX_INPUTS);
    	
    //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
    /**** Read input images ****/
		
    // For CORRCOEFF we must do part of what mars_setup does for the INP parameter...
    // ________________________________________________________________________
    /*
    char **c_filenames = new char *[MAX_INPUTS];
    if (c_filenames == NULL)
    {
        zvmessage("Memory error in setup, corrcoeff filename array", "");
        zabend();
    }
    mars_get_filelist("CORRCOEFF", c_nids, c_filenames, MAX_INPUTS, FALSE);
	
    cout << "c_nids = " << c_nids << endl;

    for (i = 0; i < c_nids; i++) 
    {
        c_file_models[i] = PigFileModel::create(c_filenames[i]);
        if (c_file_models[i] == NULL) 
        {
            snprintf(msg, msgLen, "Unable to create file model for CORRCOEFF input %d", i);
            zvmessage(msg, "");
            zabend();
        }
    }
    */
    // ________________________________________________________________________

    //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

    snprintf(msg, msgLen, "Finding disparity line and sample errors using the %s coordinate frame",
			cs->getFrameName());
    zvmessage(msg, "");

    if (nids != 2) {
	zvmessage("MARSERRDISP requires 2 and only 2 image inputs", "");
	zabend();
    }
	
    //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
    // Open input file(s). Input left and right images should be
    // 2 single band files
	
    open_inputs2_allow_different_dimensions(nids, file_models, inp_unit, inp_band, "INP");
	
    // Correlation Coefficients file is a single-band file.
	
    //open_inputs1(c_nids, c_file_models, c_unit, c_band, "CORRCOEFF");
		
    //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    PigMission *m = PigMission::getMissionObject(mission);

    /*if (c_file_models[0]->getNL() != file_models[0]->getNL() || c_file_models[0]->getNS() != file_models[0]->getNS())
    {
        zvmessage("Size of correlation coefficients file must match first input", "");
        zabend();
    }*/	
	
    // Open output files.
    // OUT can be 1 or 3 files, for a single, 3-banded file, or 3 single-band
    // files.  ERR_FILE is an error output file.

    zvpcnt("OUT", &count);
    if (count == 1)
    {
        zvunit(&out_unit[0], "OUT", 1, NULL);
        zvopen(out_unit[0], "op", "write",
            "u_ns", file_models[0]->getNS(), "u_nl",file_models[0]->getNL(),
            "u_nb", 2,
            "open_act", "sa", "u_org", "bsq",
            "u_format", "doub", "o_format", "real", NULL);
        zvplabel(out_unit[0], 0, 1);
        out_unit[1] = out_unit[0];
        out_band[0] = 1;
        out_band[1] = 2;
	
        // write output label
        PigLabelModel *labelModel = NULL;
        if( m != NULL )
        {  labelModel = m->createLabelModel(out_unit[0]);  }
        // pick the coordinate system to use.
        if ( labelModel != NULL )
            labelModel->setError(file_models, nids, cs, error_params, error_params_cnt, "DISPARITY_ERROR_MAP");
            //labelModel->setError(c_file_models, c_nids, cs, error_params, error_params_cnt, "DISPARITY_ERROR_MAP");
    }
    else if (count == 2)
    {
        char* image_type[2] = {"DISPARITY_LINE_ERROR_MAP", "DISPARITY_SAMPLE_ERROR_MAP"};
        for (i=0; i<2; i++)
        {
            zvunit(&out_unit[i], "OUT", i+1, NULL);
            zvopen(out_unit[i], "op", "write",
                "u_ns", file_models[0]->getNS(), "u_nl",file_models[0]->getNL(),
                "u_nb", 1,
                "open_act", "sa", "u_org", "bsq",
                "u_format", "doub", "o_format", "real", NULL);
            zvplabel(out_unit[0], 0, 1);
            out_band[i] = 1;

            // write output label
            PigLabelModel *labelModel = NULL;
            if( m != NULL )
            {  labelModel = m->createLabelModel(out_unit[i]);  }
            // pick the coordinate system to use.
            if ( labelModel != NULL )
                labelModel->setError(file_models, nids, cs, error_params, error_params_cnt, image_type[i]);
                //labelModel->setError(c_file_models, c_nids, cs, error_params, error_params_cnt, image_type[i]);
        }
    }
    else
    {
        zvmessage("OUT must have 1 or 3 filenames", "");
        zabend();
    }
	
    nl = file_models[0]->getNL();
    ns = file_models[0]->getNS();


    cout << "Compression method and rate" << endl;
    const char *CmprsName = file_models[0]->getCompressionName();
    const char *icer = "icer";
    const char *icer2 = "ICER ADAPTIVE VARIABLE-LENGTH CODING (ICER)";
    
    printf("file: %s\n", file_models[0]->getFilename());

    float CmprsRate;
    CmprsRate = file_models[0]->getCompressionRate(99.9);

    cout << endl << "Compression Rate: " << CmprsRate << endl << endl;
    printf("Compression Name: %s\n", ((CmprsName != NULL) ? CmprsName : "NULL"));

    cout << endl << "Compression Name: " << (CmprsName!=NULL ? CmprsName:"NULL") << endl << endl;

//    if( CmprsName == NULL )

//    else
      if( strcasecmp(CmprsName, icer) == 0 )
      { cout << "icer compression" << endl; }

      if( strcasecmp(CmprsName, icer2) == 0 )
      { cout << "icer compression" << endl; }
	
    // Allocate working images

    I1.alloc(nl, ns);
    //I2.alloc(nl, ns);		
    ci.alloc(nl, ns);
	
    // Allocate output disparity line and sample error measures
	
    dli.alloc(nl,ns);
    dsi.alloc(nl,ns);
	
    disp_unc.alloc(nl, ns);

    //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
    disp_unc_unit = -1;
    zvp("DISP_UNC", filename, &count);
    if (count == 1)
    {
        zvunit(&disp_unc_unit, "DISP_UNC", 1, "u_name", filename, NULL);
	
        zvmessage("Before DISP_UNC open", "");
        zvopen(disp_unc_unit, "op", "write",
            "u_ns", file_models[0]->getNS(), "u_nl",file_models[0]->getNL(),
            "u_nb", 1,
            "open_act", "sa", "u_org", "bsq",
            "u_format", "doub", "o_format", "real", NULL);
        zvmessage("After DISP_UNC open", "");
        zvplabel(disp_unc_unit, 0, 1);
    }
	
    //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		
    // Read INP data
	
    for (j=0; j < file_models[0]->getNL(); j++)		// line
    {		
        //         ***** READ a line from INP file *****
        // read the line from the two single band files
        		
        zvread(inp_unit[0], I1.linePtr(j), "LINE", j+1, "BAND", inp_band[0], NULL);
        //zvread(inp_unit[1], I2.linePtr(j), "LINE", j+1, "BAND", inp_band[1], NULL);
		
        for( i=0; i < file_models[0]->getNS(); i++ )		// samp
        {			

            // Print out some values
//          if (count++ < 10) {
//              snprintf(msg, msgLen, "%6.2f %6.2f %6.2f %6.2f %f %f %f %f",
//              orig_l, orig_s, disp_l, disp_s,
//              xyz.getX(), xyz.getY(), xyz.getZ(), error_value);
//              zvmessage(msg," ");
//          }
			
// #if 0	// for debug
            //if( i == 300 && j == 200 )	{	cout << I1.get(j,i) << " " << I2.get(j,i) << endl;	}
            //if( i == 200 && j == 300 )	{	cout << I1.get(j,i) << " " << I2.get(j,i) << endl;	}
            //if( i == 512 && j == 512 )	{	cout << I1.get(j,i) << " " << I2.get(j,i) << endl;	}
//#endif
        }
    }
	
    /*
    zvmessage("Reading Correlation Coefficient file...", "");
    for (j=0; j < nl; j++)		// line
    {
        zvread(c_unit, ci.linePtr(j), "LINE", j+1, "BAND", c_band, NULL);
    }
	
    i = 300, j = 200;
    cout << "I1(" << j << "," << i << ") = " << I1.get(j,i) << endl; // ", I2(" << j << "," << i << ") = " << I2.get(j,i) << endl;
    cout << "ci(" << j << "," << i << ") = " << ci.get(j,i) << endl;

    i = 200, j = 300;
    cout << "I1(" << j << "," << i << ") = " << I1.get(j,i) << endl; // ", I2(" << j << "," << i << ") = " << I2.get(j,i) << endl;
    cout << "ci(" << j << "," << i << ") = " << ci.get(j,i) << endl;

    i = 512, j = 512;
    cout << "I1(" << j << "," << i << ") = " << I1.get(j,i) << endl; // ", I2(" << j << "," << i << ") = " << I2.get(j,i) << endl;
    cout << "ci(" << j << "," << i << ") = " << ci.get(j,i) << endl;
    */

    //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    // *** Compute Inverse Scene Activity Coefficient ***
	
    double noise_var = 1.0;
	
    zvmessage("Computing Inverse Scene Activity Coefficients (ISA) ...", "");

    disp_unc = compute_disp_unc( noise_var, I1, nl, ns, wsize );

    // *** Compute Disparity Line and Sample Errors based on ISA and Compression Rate ***

    zvmessage("Computing Disparity Line and Sample Errors based on ISA and Compression Rate...", "");

    compute_DispErrors( disp_unc, CmprsRate, nl, ns, ls_order, dli, dsi );

    //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
    // Write out disparity uncertainty image:
    if (disp_unc_unit >= 0)
    {
        for (j=0; j < nl; j++)
        {
            zvwrit(disp_unc_unit, disp_unc.linePtr(j), "LINE", j+1, NULL);
        }
    }
	
    if (disp_unc_unit >= 0)
        zvclose(disp_unc_unit, NULL);
    //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    I1.free();
    //I2.free();
    ci.free();	
    disp_unc.free();
	
    // Write out the disparity line and disparity sample error:
    for (j=0; j < nl; j++)
    {
        zvwrit(out_unit[0], dli.linePtr(j), "LINE",j+1, "BAND",out_band[0],NULL);
        zvwrit(out_unit[1], dsi.linePtr(j), "LINE",j+1, "BAND",out_band[1],NULL);
    }

    zvclose(out_unit[0], NULL);
    if (out_unit[1] != out_unit[0])
        zvclose(out_unit[1], NULL);
	
    dli.free();
    dsi.free();
}

////////////////////////////////////////////////////////////////////////

void open_inputs(int nids, PigFileModel *file_models[], int unit[3], int band[3], char *type )
{
    int i;
    const size_t msgLen = 256;
    char msg[msgLen];
	
    if (nids == 1)
    {
        // FILE is OPEN with U_FORMAT of xxx to match our buffer.
		
        // get Unit id
        unit[0] = file_models[0]->getUnit();
		
        if (file_models[0]->isFileOpen())
			file_models[0]->closeFile();
        zvopen(unit[0], "op", "read", "open_act", "sa", "io_act", "sa", "u_format", "doub", NULL);
        file_models[0]->setFileOpen(TRUE);
		
		
        if (file_models[0]->getNB() != 3)
        {
            snprintf(msg, msgLen, "A single %s file must have three bands", type);
            zvmessage(msg, "");
            zabend();
        }
		
        // Initialize xyz_unit array
        unit[2] = unit[1] = unit[0];
		
        // Initialize band array
        band[0] = 1;
        band[1] = 2;
        band[2] = 3;
    }
    else if (nids == 3)
    {
        for (i = 0; i < 3; i++)
        {
            // make sure that file is open
            if (file_models[i]->isFileOpen())
                file_models[i]->closeFile();
			
            // get Unit id
            unit[i] = file_models[i]->getUnit();
			
            zvopen(unit[i], "op", "read", "open_act", "sa", "io_act", "sa", "u_format", "doub", NULL);
            file_models[i]->setFileOpen(TRUE);
			
            if (file_models[i]->getNB() != 1)
            {
                snprintf(msg, msgLen, "A three-file %s must have one band each", type);
                zvmessage(msg, "");
                zabend();
            }
			
            // check that all files are the same size
            if ((file_models[i]->getNL() != file_models[0]->getNL()) || (file_models[i]->getNS() != file_models[0]->getNS()))
            {
                zvmessage("Input is of different size than Input #1", "");
                zabend();
            }
            band[i] = 1;
        }
    }
    else
    {
        snprintf(msg, msgLen, "open_inputs requires either 1 3-band file or 3 single band files as input for %s", type);
        zvmessage(msg, "");
        zabend();
    }
}

///////////////////////////////////////////////////////////////////////////////////

void open_inputs2_allow_different_dimensions(int nids, PigFileModel *file_models[], int unit[2], int band[2], char *type )
{
    int i;
    const size_t msgLen = 256;
    char msg[msgLen];
	
    if (nids == 2)
    {
        for (i = 0; i < 2; i++)
        {
            // make sure that file is open
            if (file_models[i]->isFileOpen())
                file_models[i]->closeFile();
			
            // get Unit id
            unit[i] = file_models[i]->getUnit();
			
            zvopen(unit[i], "op", "read", "open_act", "sa", "io_act", "sa", "u_format", "doub", NULL);
            file_models[i]->setFileOpen(TRUE);
			
            if (file_models[i]->getNB() != 1)
            {
                snprintf(msg, msgLen, "A two-file %s must have one band each", type);
                zvmessage(msg, "");
                zabend();
            }
			
            // check that all files are the same size
            if ((file_models[i]->getNL() != file_models[0]->getNL()) || (file_models[i]->getNS() != file_models[0]->getNS()))
            {
                zvmessage("Input is of different size than Input #1", "");
                zvmessage("Different size of stereo pair is allowed in this program", "");
                // zabend();
            }
            band[i] = 1;
        }
    }
    else
    {
        snprintf(msg, msgLen, "open_inputs2 requires 2 single band files as input for %s", type);
        zvmessage(msg, "");
        zabend();
    }
}

///////////////////////////////////////////////////////////////////////////////////

void open_inputs1(int nids, PigFileModel *file_models[], int& unit, int& band, char *type )
{
    const size_t msgLen = 256;
    char msg[msgLen];
		
    if (nids == 1)
    {
		// make sure that file is open
		if (file_models[0]->isFileOpen())
			file_models[0]->closeFile();
		
		// get Unit id
		unit = file_models[0]->getUnit();
			
		zvopen(unit, "op", "read", "open_act", "sa", "io_act", "sa", "u_format", "doub", NULL);
		file_models[0]->setFileOpen(TRUE);
		
		if (file_models[0]->getNB() != 1)
		{
			snprintf(msg, msgLen, "A file %s must have one band each", type);
			zvmessage(msg, "");
			zabend();
		}
		
		band = 1;
    }
    else
    {
        snprintf(msg, msgLen, "open_inputs1 requires one single-band file as input for %s", type);
        zvmessage(msg, "");
        zabend();
    }
}

// ################################################################################

SimpleImage<double> compute_disp_unc( const double noise_var, SimpleImage<double>& I, const int nl, const int ns, const int wsize )
{
    const size_t msgLen = 150;
	char msg[msgLen];
	int i, j, ii, jj;
	
	int a, b;
	double c, d;
	
	double sum;     // sum of gradients over correlation window
	double value;   // disparity variance value
	SimpleImage<double> disp_unc;
	
	snprintf(msg, msgLen, "Correlation search window for disparity uncertainty computation:: %d", wsize);
	zvmessage(msg, "");
		
	disp_unc.alloc(nl, ns);
	
	int w = (wsize-1)/2;
		
	for (j=w; j < nl-w-1; j++)
	{
		for (i=w; i < ns-w-1; i++)
		{
			sum = 0.0;
			
			for( jj=-w; jj <= w; jj++ )
			{
				for( ii=-w; ii <= w; ii++ )
				{
					a = i + ii;
					b = j + jj;
					c = I.get(b,a+1)-I.get(b,a);
					d = I.get(b+1,a)-I.get(b,a);
					sum = sum + c*c + d*d;
				}
			}
			
			value = noise_var / sum;
			disp_unc.set(j,i,value);
		}
	}
	
	return disp_unc;
	
}

// ################################################################################

SimpleImage<double> compute_curv( SimpleImage<double>& f, const int nl, const int ns )
{
        //
        // THIS FUNCTION IS CURRENTLY NOT USED!
        //

	zvmessage("Computing Curvature", "");

	int i, j;
	
	double value;   // curvature value
	SimpleImage<double> curv;
	
	curv.alloc(nl, ns);
	
	for (j=2; j < nl; j++)
	{
		for (i=2; i < ns; i++)
		{
			value = f.get(j+1,i) + f.get(j-1,i) + f.get(j,i+1) + f.get(j,i-1) - 4.0*f.get(j,i);
			curv.set(j,i,value);
			
		}
	}
	
	return curv;
	
}

// ################################################################################

void compute_DispErrors( SimpleImage<double>& isa, const float BPP, const int nl, const int ns, const int LS_order, SimpleImage<double>& dl, SimpleImage<double>& ds )
{
    // (1) Given the Inverse Scene Activity map, compute line disparity and sample
    //     disparity errors.
    // (2) Add standard deviation in order to obtain 1-sigma error.
    // (3) Multiply 1-sigma error by compression factor.
	
    // Details:
    //
    // (1) Ordering of polynomial coefficients are from constant term 
    //     to the highest term, i.e. a(0) + a(1)*x + a(2)*x^2 + ...
    //     Coefficients were calculated outside this program.  Multiple images
    //     were analyzed in order to determine coefficients.
        
    //     aL are coefficients of a polynomial fitting Dispariy Line errors (L) 
    //      versus a quantity, such as isa

    //     aS are coefficients of a polynomial fitting Dispariy Sample errors (S) 
    //      versus a quantity, such as isa

    // (2) Standard deviation was calculated outside this program.
        
    // (3) Compression coefficients were calculated outside this program.


    // arrays are 0-based:
    //   [4] corresponds to no compression
    //   [3] to 4 bpp 
    //   [2] to 3 bpp
    //   [1] to 2 bpp
    //   [0] to 1 bpp

    double mean_ll[5];
    double mean_ss[5];

    mean_ll[4] = 0.082401079355061;   // assumes no or nearly no compression
    mean_ll[3] = 0.082430048956342;   // 4 bpp
    mean_ll[2] = 0.082544158295169;   // 3 bpp
    mean_ll[1] = 0.082816020782462;   // 2 bpp
    mean_ll[0] = 0.084674614433785;   // 1 bpp

    mean_ss[4] = 0.109650591643856;   // assumes no or nearly no compression
    mean_ss[3] = 0.109749136097506;   // 4 bpp
    mean_ss[2] = 0.109786293350872;   // 3 bpp
    mean_ss[1] = 0.110342451665708;   // 2 bpp
    mean_ss[0] = 0.113061653154166;   // 1 bpp


    // Compression factors tell by how much an error estimate should be multiplied
    // if compression information is taken into account:
    double cf_ll[5];
    double cf_ss[5];


    long bpp;
    for( bpp = 4; bpp >= 0; bpp-- )
    {
        cf_ll[bpp] = mean_ll[bpp]/mean_ll[4];
        cf_ss[bpp] = mean_ss[bpp]/mean_ss[4];
    }

    cout << "BPP = " << BPP << endl;
    for( bpp = 4; bpp >= 0; bpp-- )
    {
        cout << "cf_ll[" << bpp << "] = " << cf_ll[bpp] << endl;
        cout << "cf_ss[" << bpp << "] = " << cf_ss[bpp] << endl;
    }

    // Calculate compression factors for the specific BPP:
    long fbpp = long(floor(BPP));
    // arrays are 0-based (need to subtract 1 from each array index):
    double comp_factor_ll = cf_ll[fbpp-1] + (cf_ll[fbpp]-cf_ll[fbpp-1])*(BPP - fbpp);
    double comp_factor_ss = cf_ss[fbpp-1] + (cf_ss[fbpp]-cf_ss[fbpp-1])*(BPP - fbpp);

    cout << "comp_factor_ll = " << comp_factor_ll << endl;
    cout << "comp_factor_ss = " << comp_factor_ss << endl;

    //double al[LS_order+1];
    //double as[LS_order+1];

    double *al = new double[LS_order+1];
    double *as = new double[LS_order+1];

    if( LS_order == 1 )		// Linear
    {
        al[0] = 0.0820;
        al[1] = 29509;

        as[0] = 0.1095;
        as[1] = 8353.5;
    }
    else
    {
        cout << "Invalid Order of Polynomial was specified" << endl;
        zabend();
    }
    //
    // Since there was no clear correlation between averaged bin StD and 
    // line/sample error, single global standard deviation value was
    // calculated.  To get 1-sigma error, this std is added to the
    // expected error.
    //
    double global_stdL = 0.0755;
    double global_stdS = 0.0919;

    int i1 = 100, j1 = 100;
    int i2 = 168, j2 = 560;
    int i3 =  40, j3 = 515;

    int i, j;
    int n;
    double c;
    double lerr, serr;
    for (j=0; j < nl; j++)
    {
        for (i=0; i < ns; i++)
        {
            c = isa.get(j,i);

            if( (i == i1 && j == j1) || (i == i2 && j == j2) || (i == i3 && j == j3) )
            {
                cout << endl << endl << "isa(" << i << "," << j << ") = " << c << endl;
            }
			
            lerr = al[0];
            serr = as[0];
            for( n=1; n <= LS_order; n++ )
            {
                lerr = lerr + al[n]*pow(c,n);
                serr = serr + as[n]*pow(c,n);
            }

            if( (i == i1 && j == j1) || (i == i2 && j == j2) || (i == i3 && j == j3) )
            {
                cout << "lerr (" << i << "," << j << ") = " << lerr << endl;
                cout << "serr (" << i << "," << j << ") = " << serr << endl;
            }

            // 1-sigma error (for not compressed images)
            lerr = lerr + global_stdL;
            serr = serr + global_stdS;

            if( (i == i1 && j == j1) || (i == i2 && j == j2) || (i == i3 && j == j3) )
            {
                cout << "lerr (" << i << "," << j << ") = " << lerr << endl;
                cout << "serr (" << i << "," << j << ") = " << serr << endl;
            }

            lerr = lerr*comp_factor_ll;
            serr = serr*comp_factor_ss;

            if( (i == i1 && j == j1) || (i == i2 && j == j2) || (i == i3 && j == j3) )
            {
                cout << "lerr (" << i << "," << j << ") = " << lerr << endl;
                cout << "serr (" << i << "," << j << ") = " << serr << endl;
            }

            dl.set(j,i,lerr);
            ds.set(j,i,serr);
        }
    }

    delete[] al;
    delete[] as;
}



void compute_DispErrors_based_on_CorrelationQuality( SimpleImage<double>& ci, const int nl, const int ns, const int LS_order, SimpleImage<double>& dl, SimpleImage<double>& ds )
{
        //
        // THIS FUNCTION IS CURRENTLY NOT USED!
        //

	// Given the correlation quality map, compute line disparity and sample
	// disparity errors.
	
	// Ordering of polynomial coefficients are from constant term 
	//  to the highest term, i.e. a(0) + a(1)*x + a(2)*x^2 + ...
	
	// aL are coefficients of a polynomial fitting Dispariy Line errors (L) 
	//  versus a quantity, such as correlation coefficients

	// aS are coefficients of a polynomial fitting Dispariy Sample errors (S) 
	//  versus a quantity, such as correlation coefficients

	// double al[LS_order+1];
	// double as[LS_order+1];


        double *al = new double[LS_order+1];
        double *as = new double[LS_order+1];

	if( LS_order == 1 )		// Linear
	{
		al[0] =  2.413369554305496;
		al[1] = -2.198208207730628;
		
		as[0] =  2.430965770745418;
		as[1] = -2.139793773172884;
	}
	else if( LS_order == 2 )	// Quadratic
	{
		al[0] =  2.061249053528706;
		al[1] = -1.366260740477683;
		al[2] = -0.483055986218572;
		
		as[0] =  1.530277191139703;
		as[1] = -0.011756408311733;
		as[2] = -1.235608290734098;
	}
	else if( LS_order == 3 )	// Cubic
	{
		al[0] =  -4.069112599400892;
		al[1] =  21.135558473253713;
		al[2] = -27.651276169411524;
		al[3] =  10.805449186004935;
		
		as[0] =  3.153005403214537;
		as[1] = -5.968066819993751;
		as[2] =  5.955914827303773;
		as[3] = -2.860240282361605;
	}
	else if( LS_order == 4 )	// Quartic
	{
		al[0] =   27.6675758909997;
		al[1] = -136.4936980834157;
		al[2] =  263.0615032812200;
		al[3] = -225.2794596625302;
		al[4] =   71.2713322124522;
		
		as[0] =   39.9025006110316;
		as[1] = -188.4949036164954;
		as[2] =  342.5869491310065;
		as[3] = -276.2349771422043;
		as[4] =   82.5287269169477;
	}
	else {
		cout << "Invalid Order of Polynomial was specified" << endl;
                zabend();
	}

	
	int i, j;
	int n;
	double c;
	double lerr, serr;
	for (j=0; j < nl; j++)
	{
		for (i=0; i < ns; i++)
		{
			c = ci.get(j,i);
			
			lerr = al[0];
			serr = as[0];
			for( n=1; n <= LS_order; n++ )
			{
				lerr = lerr + al[n]*pow(c,n);
				serr = serr + as[n]*pow(c,n);
			}
			
			dl.set(j,i,lerr);
			ds.set(j,i,serr);
			
		}
	}

        delete[] al;
        delete[] as;
}

// ################################################################################


void get_DispErrors_from_CompressionRate( const int CR, double& dl, double& ds )
{
        //
        // THIS FUNCTION IS CURRENTLY NOT USED!
        //

	// Given the Compression Rate (CR) value, return expected line disparity 
	// and sample disparity errors.
	
	if( CR == 5 )	// not compressed
	{
		dl = 0.0770;
		ds = 0.1039;
	}
	else if ( CR == 4 ) // 4bpp
	{
		dl = 0.1063;
		ds = 0.1432;
	}
	else if ( CR == 3 )	// 3bpp
	{
		dl = 0.2140;
		ds = 0.4303;
	}
	else if ( CR == 2 )
	{
		dl = 1.4752;
		ds = 7.4804;
	}
	else if ( CR == 1 )
	{
		dl = 5.9665;
		ds = 29.9354;
	}
	else {
	    zvmessage("Invalid Compression Rate!", "");
            zabend();
	}
}

