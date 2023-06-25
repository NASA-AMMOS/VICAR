/* marsslope */
#include "vicmain_c"

#include "mars_support.h"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigSurfaceModel.h"
#include "PigVector.h"
#include "RadiometryModel.h"
#include "PigCoordSystem.h"
#include "PigCSReference.h"
#include "mat3.h"

#include "return_status.h"

#include <math.h>

#include "SlopeFunction.h"


/* buffer sizes in main program */
#define MAX_INPUTS 3
#define MAX_OBUF 30000


//void open_inputs(int nids, PigFileModel *file_models[], int unit[3], int band[3], char *type, char *UFORMAT);
void open_inputs( int nids, PigFileModel *file_models[], int unit[3], int band[3], char *type );

////////////////////////////////////////////////////////////////////////

void main44()
{
    int i, j;
    int count;
    const int MSG_LEN = 150;
    char msg[MSG_LEN];

    int xyz_nids, uvw_nids;
    char mission[64], instrument[64];
    int xyz_band[3], uvw_band[3];

    // Inputs

    PigFileModel *xyz_file_models[MAX_INPUTS], *uvw_file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    int homogeneous_inputs = TRUE;
    PigCoordSystem *cs = NULL, *xyz_cs, *uvw_cs;
    int xyz_unit[3], uvw_unit[3];
    float point[3];

    // Outputs
    int unit_out;
    int nlo, nso;
    double *S;
    double *x, *y, *z;
    double *u, *v, *w;


    // User Parameters
    //char* sf_name;			// string specifying a slopeFunction to be calculated
    char sf_name[10];			// string specifying a slopeFunction to be calculated
    float sa = 0.0;			// solar angle

    zvmessage("MARSSLOPE version 2020-05-19", "");

    // Get the input file list, and set up initial camera/pointing models
    // for each input.  Although we accept one or three inputs only, mars_setup
    // does lots of other nice things for us.

    mars_setup(xyz_nids, xyz_file_models, camera_in, pointing_in, NULL, cs,
                mission, instrument, homogeneous_inputs,
                0, 0, MAX_INPUTS);

    PigMission *m = PigMission::getMissionObject(mission);

    S = new double[xyz_file_models[0]->getNS()];
    x = new double[xyz_file_models[0]->getNS()];
    y = new double[xyz_file_models[0]->getNS()];
    z = new double[xyz_file_models[0]->getNS()];
    u = new double[xyz_file_models[0]->getNS()];
    v = new double[xyz_file_models[0]->getNS()];
    w = new double[xyz_file_models[0]->getNS()];

    if (S == NULL || x == NULL || y == NULL || z == NULL ||
		u == NULL || v == NULL || w == NULL) {
	zvmessage("Memory allocation error", "");
	zabend();
    }

    // For UVW we must do part of what mars_setup does for the INP parameter...
    // ________________________________________________________________________
    char **uvw_filenames = new char *[MAX_INPUTS];
    if (uvw_filenames == NULL)
    {
        zvmessage("Memory error in setup, uvw filename array", "");
        zabend();
    }
    mars_get_filelist("UVW", uvw_nids, uvw_filenames, MAX_INPUTS, FALSE);

    for (i = 0; i < uvw_nids; i++) 
    {
        uvw_file_models[i] = PigFileModel::create(uvw_filenames[i]);
        if (uvw_file_models[i] == NULL) 
        {
            snprintf(msg, MSG_LEN, "Unable to create file model for UVW input %d", i);
            zvmessage(msg, "");
            zabend();
        }
    }
    // ________________________________________________________________________

    // Get coord system for input XYZ file

    PigCSReference *ref;
    xyz_file_models[0]->getDerivedImageCS(ref);
    xyz_cs = m->getCoordSystem(ref);

    snprintf(msg, MSG_LEN, "Interpreting XYZ values using the %s coordinate frame: %s",
		xyz_cs->getFrameName(), ref->getFullName());
    zvmessage(msg, "");

    // Get coord system for input UVW file

    uvw_file_models[0]->getDerivedImageCS(ref);
    uvw_cs = m->getCoordSystem(ref);

    snprintf(msg, MSG_LEN, "Interpreting UVW values using the %s coordinate frame: %s",
                uvw_cs->getFrameName(), ref->getFullName());
    zvmessage(msg, "");
    // ________________________________________________________________________

    // Get the coordinate system to use.
    snprintf(msg, MSG_LEN, "Generating SLOPE CALCULATIONS using the %s coordinate frame. \n",
	    cs->getFrameName());
    zvmessage(msg, "");

    // Get slopeFunction to use
    zvpcnt("type", &count);
    zvp("type", sf_name, &count);

    // Create a pointer to SlopeFunction object
    SlopeFunction *sf = SlopeFunction::getSlopeFunctionObject( sf_name );


    // Check if Origin is used in calculations of a given SlopeFunction type.
    // Get Origin to use
    if( sf->usesOrigin() )
    {
        zvpcnt("origin", &count);
        if( count == 3 )
        {
            zvp("origin", point, &count);
            sf->sfOrigin.setXYZ(point);
        }
        else
	{
	    // Use rover location as a default range origin value 
	    // We get this by using (0,0,0) in the XYZ file's Rover frame
	    // and converting it to the cs we're using.
	    // This use of "ROVER" is somewhat mission-dependent...
	    PigCoordSystem *cs_rover = m->getCoordSystem(
			xyz_file_models[0], "ROVER");
	    if (cs_rover == NULL) {
		zvmessage("ERROR - Origin not defined and ROVER frame not found", "");
		zabend();
	    }
	    PigPoint zero(0.0, 0.0, 0.0);
            sf->sfOrigin = cs->convertPoint(zero, cs_rover);
	}
	
        snprintf( msg, MSG_LEN, "Using POINT (%f, %f, %f) as range origin", sf->sfOrigin.getX(), sf->sfOrigin.getY(), sf->sfOrigin.getZ() );
		zvmessage(msg, "");
    }

    // Get solar angle
    //zvpcnt("sa", &count);

    // Check if Solar Angle is used in calculations of a given SlopeFunction type.
    // Get Solar Angle to use
    if( sf->usesSA() )
    {
        zvp("sa", &sa, &count);
        sf->setSolarAngle( sa );
		
        snprintf(msg, MSG_LEN, "Solar Angle: count = %d", count);
        zvmessage(msg, "");
		
        snprintf( msg, MSG_LEN, "Using Solar Angle = %f", sa );
        zvmessage(msg, "");
    }

    snprintf(msg, MSG_LEN, "Product name: %s", sf->getSlopeFunctionName() );
    zvmessage(msg, "");

    snprintf(msg, MSG_LEN, "Calculating %s", sf->getPrintName());
    zvmessage(msg, "");

    // Open input file(s).  An XYZ data could be either 1 3-band file
    // or 3 single band files

    //open_inputs(xyz_nids, xyz_file_models, xyz_unit, xyz_band, "XYZ", "real");
    open_inputs(xyz_nids, xyz_file_models, xyz_unit, xyz_band, "XYZ");

    // Now open the UVW's properly

    //open_inputs(uvw_nids, uvw_file_models, uvw_unit, uvw_band, "UVW", "real");
    open_inputs(uvw_nids, uvw_file_models, uvw_unit, uvw_band, "UVW");

    if ((xyz_file_models[0]->getNL() != uvw_file_models[0]->getNL()) || (xyz_file_models[0]->getNS() != uvw_file_models[0]->getNS()))
    {
        zvmessage("XYZ and UVW files are not the same size", "");
        zabend();
    }
    // ________________________________________________________________________


    // Open output files.
    // OUT should be 1 file
    zvpcnt("OUT", &count);
    if (count != 1)
    {
        zvmessage("OUT must have 1 filename", "");
        zabend();
    }

    count = 0;

    // get input image dimensions
    nlo = xyz_file_models[0]->getNL();
    nso = xyz_file_models[0]->getNS();
    snprintf(msg, MSG_LEN, "Output # lines & samples=%10d %10d \n", nlo, nso);
    zvmessage(msg, "");

    // check for limits
    if ((nlo > MAX_OBUF) || (nlo < 1) || (nso > MAX_OBUF) || (nso < 1))
    {
        zvmessage("Unreasonable output file dimensions", "");
        zabend();
    }

    zvselpiu(xyz_file_models[0]->getUnit());	  // transfer labels
    zvunit(&unit_out, "OUT", 1, NULL);

    /*status = zvopen(unit_out, "OP", "WRITE", "U_FORMAT", "REAL",
               "U_NS", nso, "U_NL", nlo, "OPEN_ACT", "AS",
               "U_NB", 1, "U_ORG", "BSQ", "O_FORMAT", "REAL", NULL);*/

    zvopen(unit_out, "OP", "WRITE", "U_FORMAT", "DOUB",
	   "U_NS", nso, "U_NL", nlo, "OPEN_ACT", "AS",
	   "U_NB", 1, "U_ORG", "BSQ", "O_FORMAT", "REAL", NULL);
 
    // Write the output labels
    PigLabelModel *labelModel = m->createLabelModel( unit_out );
    PigPoint *pt = NULL;
    if (sf->usesOrigin())
        pt = &sf->sfOrigin;
    // gather all filemodels for handing over to label model
    int nids = xyz_nids + uvw_nids;
    PigFileModel *file_models[MAX_INPUTS*2];
    for (int cnt = 0; cnt < xyz_nids; cnt++)
        file_models[cnt] = xyz_file_models[cnt];
    for (int cnt = 0; cnt < uvw_nids; cnt++)
        file_models[xyz_nids+cnt] = uvw_file_models[cnt];
    labelModel->setSlope( file_models, nids, cs, pt, sa, sf->getLabelName() );
    zvplabel( unit_out, 0, 1 );


    // Process XYZ and UVW data

    for (j=0; j < uvw_file_models[0]->getNL(); j++)		// line
    {		

        //         ***** READ a line from XYZ file *****
        //read the line from the three bands of the single input file
        //or read the line from the three single band files
        zvread(xyz_unit[0], x, "LINE", j+1, "BAND", xyz_band[0], NULL);
        zvread(xyz_unit[1], y, "LINE", j+1, "BAND", xyz_band[1], NULL);
        zvread(xyz_unit[2], z, "LINE", j+1, "BAND", xyz_band[2], NULL);

        //         ***** READ a line from UVW file *****
        //read the line from the three bands of the single input file
        //or read the line from the three single band files
        zvread(uvw_unit[0], u, "LINE", j+1, "BAND", uvw_band[0], NULL);
        zvread(uvw_unit[1], v, "LINE", j+1, "BAND", uvw_band[1], NULL);
        zvread(uvw_unit[2], w, "LINE", j+1, "BAND", uvw_band[2], NULL);


        for( i=0; i < uvw_file_models[0]->getNS(); i++ )		// samp
        {
            // 0.0 is being used as a specific flag value and is representable exactly
            if ( x[i] == 0.0 && y[i] == 0.0 && z[i] == 0.0 )          // invalid point
            {    S[i] = 0.0;    }
            else if ( u[i] == 0.0 && v[i] == 0.0 && w[i] == 0.0 )     // invalid point
            {    S[i] = 0.0;    }
            else
            {
                PigPoint xyz( x[i], y[i], z[i] );
                PigVector uvw( u[i], v[i], w[i] );

                if (cs != xyz_cs)    // Convert coord sys's
                {
                    PigPoint new_xyz = cs->convertPoint( xyz, xyz_cs );
                    xyz = new_xyz;
                }
                if (cs != uvw_cs)    // Convert coord sys's
                {
                    PigVector new_uvw = cs->convertVector( uvw, uvw_cs );
                    uvw = new_uvw;
                }
				
                // Perform a computation by calling evaluate() member function
                // of an appropriate subclass of SlopeFunction:
                    S[i] = sf->evaluate( xyz, uvw );
                }
#if 0	// for debug
                if( i == 300 && j == 200 )	{	cout << u[i] << " " << v[i] << " " << w[i] << "  Number = " << S[i] << " " << endl;	}
                if( i == 200 && j == 300 )	{	cout << u[i] << " " << v[i] << " " << w[i] << "  Number = " << S[i] << " " << endl;	}
                if( i == 512 && j == 512 )	{	cout << u[i] << " " << v[i] << " " << w[i] << "  Number = " << S[i] << " " << endl;	}
#endif
            }
            zvwrit(unit_out, S, "LINE", j+1, "BAND", 1, NULL);
    }
    zvclose(unit_out, NULL);
}


////////////////////////////////////////////////////////////////////////
// Open one set of inputs, either XYZ or UVW...
////////////////////////////////////////////////////////////////////////

void open_inputs(int nids, PigFileModel *file_models[], int unit[3], int band[3], char *type )
{
    int i;
    const int MSG_LEN = 256;
    char msg[MSG_LEN];

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
            snprintf(msg, MSG_LEN, "A single %s file must have three bands", type);
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
                snprintf(msg, MSG_LEN, "A three-file %s must have one band each", type);
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
        snprintf(msg, MSG_LEN, "MARSSLOPE requires either 1 3-band file or 3 single band files as input for %s", type);
        zvmessage(msg, "");
        zabend();
    }
}

//#####################################################################
