/* mslrough */
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
#include <stdlib.h>

#include "xyz_uvw_to_roughness.h"

#ifdef _OPENMP
#include <omp.h>
#endif

#include <iostream>
using namespace std;

/* buffer sizes in main program */
#define MAX_INPUTS 3
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS

void open_inputs(int nids, PigFileModel *file_models[], int unit[3],
		int band[3], char *type);

////////////////////////////////////////////////////////////////////////

void main44()
{
    int i, j, band, line;
    int status, count, def;
    const size_t msgLen = 256;
    char msg[msgLen];

    int nids = 0;
    int uvw_nids = 0;
    char mission[64], instrument[64];

    // Inputs

    PigFileModel *file_models[MAX_INPUTS], *uvw_file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    int homogeneous_inputs = TRUE;
    PigCoordSystem *cs, *xyz_cs, *uvw_cs;
    int xyz_unit[3], uvw_unit[3];
    int xyz_band[3], uvw_band[3];

    // Outputs
    int out_unit;
    int out_band;
    int nlo, nso;

    // User Parameters
    XyzUvwToRoughnessParams params;
    int do_ring;
 
    double *xyz[3];                // input image
    double *uvw[3];                // input image
    double uvw_vector[3];          // input used if single UVW vector
    double *out_data[3];           // output image

    zvmessage("MSLROUGH version 3", "");

    // Get the input file list, and set up initial camera/pointing models
    // for each input.  Although we accept one or three inputs only, mars_setup
    // does lots of other nice things for us.

    mars_setup(nids, file_models, camera_in, pointing_in, NULL, cs,
                mission, instrument, homogeneous_inputs,
                MAX_NL, MAX_NS, MAX_INPUTS);

    PigMission *m = PigMission::getMissionObject(mission);

    // A boolean parameter to check to find out whether UVW data is provided
    // in file form (matrix of vectors) or at command line (a single vector).

    zvpcnt("UVW", &count);
    int uvw_is_matrix = (count > 0);

    char **uvw_filenames = NULL;
    if (uvw_is_matrix) {
        zvmessage("Reading UVW vectors from file(s), ignoring UVW_VECTOR and UVW_COORD!", "");

        // For UVW we must do part of what mars_setup does for the INP parameter...

        uvw_filenames = new char *[MAX_INPUTS];
        if (uvw_filenames == NULL) {
            zvmessage("Memory error in setup, uvw filename array", "");
            zabend();
        }
        mars_get_filelist("UVW", uvw_nids, uvw_filenames, MAX_INPUTS, FALSE);

        for (i = 0; i < uvw_nids; i++) {
            uvw_file_models[i] = PigFileModel::create(uvw_filenames[i]);
            if (uvw_file_models[i] == NULL) {
                snprintf(msg, msgLen, "Unable to create file model for UVW input %d", i);
                zvmessage(msg, "");
                zabend();
            }
        }
    } else {
        zvmessage("Getting a single UVW vector info from UVW_VECTOR and UVW_COORD!", "");

        zvparmd("UVW_VECTOR", uvw_vector, &count, &def, 3, 0);
        if (count != 3) {
            zvmessage("No complete UVW 3D surface-normal vector(s) provided at all!", "");
            zabend();
        }
    }

    // Get coord system for input XYZ file

    PigCSReference *ref;
    file_models[0]->getDerivedImageCS(ref);
    xyz_cs = m->getCoordSystem(ref);

    snprintf(msg, msgLen, "Interpreting XYZ values using the %s coordinate frame: %s",
	    xyz_cs->getFrameName(), ref->getFullName());
    zvmessage(msg, "");

    // Get coord system for input UVW file

    if (uvw_is_matrix) {
        PigCSReference *ref2;
        uvw_file_models[0]->getDerivedImageCS(ref2);
        uvw_cs = m->getCoordSystem(ref2);

        snprintf(msg, msgLen, "Interpreting UVW values using the %s coordinate frame: %s",
                    uvw_cs->getFrameName(), ref2->getFullName());
        zvmessage(msg, "");
    } else {
        char frame[64] = "";
        zvp("UVW_COORD", frame, &count);
        uvw_cs = m->getCoordSystem(file_models[0], frame);

        snprintf(msg, msgLen, "Interpreting UVW values using the %s coordinate frame.",
                    uvw_cs->getFrameName());
        zvmessage(msg, "");
    }

    // Get parameters

    zvparmd("INNER_RADIUS", &params.ring_inner_radius, &count, &def, 1, 0);
    zvparmd("OUTER_RADIUS", &params.ring_outer_radius, &count, &def, 1, 0);
    zvparmd("PATCH_RADIUS", &params.patch_radius, &count, &def, 1, 0);
    if(count == 0)
    {
        params.patch_radius = params.ring_outer_radius;
    }

    zvparmd("SPHERE_RADIUS", &params.sphere_cull_radiusSq, &count, &def, 1, 0);
    if(params.sphere_cull_radiusSq > 0)
    {
        snprintf(msg, msgLen, "Using optional sphere culling with radius: %f",params.sphere_cull_radiusSq);
        zvmessage(msg, "");

        //square it for fast testing later
        params.sphere_cull_radiusSq *= params.sphere_cull_radiusSq;
    }

    zvparmd("ROUGH_RING", &params.max_roughness_ring, &count, &def, 1, 0);
    zvparmd("ROUGH_OVERALL", &params.max_roughness_patch, &count, &def, 1, 0);
    zvparmd("BAD_ROUGH", &params.bad_roughness, &count, &def, 1, 0);
    zvparmd("FILTER_SCALE", &params.filter_scale, &count, &def, 1, 0);
    zvp("MAX_WINDOW", &params.max_window_size, &count);
    zvp("MIN_POINTS", &params.min_close_points, &count);
    zvparmd("X_CENTER", &params.x_center, &count, &def, 1, 0);
    zvparmd("Y_CENTER", &params.y_center, &count, &def, 1, 0);
    zvparmd("BOX_RADIUS", &params.box_radius, &count, &def, 1, 0);
    zvparmd("BAD_CURVE", &params.bad_curvature, &count, &def, 1, 0);  
    zvparmd("CONVEX_HIGH", &params.convexity_threshold_high, &count, &def, 1, 0);
    zvparmd("CONVEX_LOW", &params.convexity_threshold_low, &count, &def, 1, 0);
    zvparmd("CONCAVE_LOW", &params.concavity_threshold_low, &count, &def, 1, 0);
    zvparmd("CONCAVE_HIGH", &params.concavity_threshold_high, &count, &def, 1, 0);
    
    params.do_curvature = zvptst("DO_CURVATURE");
    do_ring = zvptst("DO_RING");

    int omp_on = zvptst("OMP_ON");
    int num_thr = 1;
    #ifdef _OPENMP
    if (omp_on) {
        num_thr = omp_get_max_threads();
        snprintf(msg, msgLen, "Using OpenMP with %d threads", num_thr);
        zvmessage(msg, "");
    }
    else
    {
        snprintf(msg, msgLen, "OpenMP multithreading disabled");
        zvmessage(msg, "");
    }
    #else
     if (omp_on) {
        snprintf(msg, msgLen, "OpenMP multithreading requested, but not enabled during build");
        zvmessage(msg, "");
     }
     else
     {
        snprintf(msg, msgLen, "No OpenMP multithreading, enabled during build");
        zvmessage(msg, "");
     }
    #endif

    // Get the coordinate system to use.
    snprintf(msg, msgLen, "Generating %s using the %s coordinate frame.",
        params.do_curvature ? "curvature" : "roughness",
	    cs->getFrameName());
    zvmessage(msg, "");

    // If we're not doing the ring, set ring param values to be equal
    // which disables the ring in the subroutine.

    if (!do_ring) {
        params.ring_inner_radius = params.patch_radius;
        params.ring_outer_radius = params.patch_radius;
        params.max_roughness_ring = params.max_roughness_patch;
    }
    else if (params.do_curvature) {
        zvmessage("Curvature is only calculated for patch, -do_ring is not allowed","");
        zabend();
    }
    else if (params.ring_inner_radius > params.ring_outer_radius) {
        zvmessage("INNER_RADIUS must be less or equal to the OUTER_RADIUS!!","");
        zabend();
    }
    
    // the 'low' threshold is the tighter constraint than the 'high' threshold
    if(params.concavity_threshold_low > params.concavity_threshold_high) {
         zvmessage("CONCAVE_LOW should be less than or equal to CONCAVE_HIGH","");
        zabend();
    }
    else if(params.convexity_threshold_low > params.convexity_threshold_high) {
         zvmessage("CONCAVE_LOW should be less than or equal to CONCAVE_HIGH","");
        zabend();
    }

    // Open input file(s).  An XYZ data could be either 1 3-band file
    // or 3 single band files

    open_inputs(nids, file_models, xyz_unit, xyz_band, "XYZ");

    // Now open the UVW's properly

    if (uvw_is_matrix) {
        open_inputs(uvw_nids, uvw_file_models, uvw_unit, uvw_band, "UVW");

        if ((file_models[0]->getNL() != uvw_file_models[0]->getNL()) ||
            (file_models[0]->getNS() != uvw_file_models[0]->getNS())) {
	    zvmessage("XYZ and UVW files are not the same size", "");
	    zabend();
        }
    }

    // Open output files.
    // OUT is a single 2- or 3-band float file
    int num_out_bands = (do_ring || params.do_curvature) ? 3 : 2;
    zvpcnt("OUT", &count);
    zvunit(&out_unit, "OUT", 1, NULL);
    zvopen(out_unit, "op", "write",
	       "u_ns", file_models[0]->getNS(), "u_nl",file_models[0]->getNL(),
	       "u_nb", num_out_bands,
	       "open_act", "sa", "u_org", "bsq",
	       "u_format", "doub", "o_format", "real", NULL);
    zvplabel(out_unit, 0, 1);

    // write output label

    PigLabelModel *labelModel = m->createLabelModel(out_unit);

    // gather all filemodels for handing over to label model
    int nids_all = nids;
    PigFileModel *file_models_all[MAX_INPUTS*2];
    for (int cnt = 0; cnt < nids; cnt++)
        file_models_all[cnt] = file_models[cnt];
    if (uvw_is_matrix) {
        nids_all += uvw_nids;
        for (int cnt = 0; cnt < uvw_nids; cnt++)
            file_models_all[nids+cnt] = uvw_file_models[cnt];
    }

    // pick the coordinate system to use.
               
     int flags = do_ring + (params.do_curvature << 1); /*0: patch, no ring (roughness)
                                                         1: patch and ring (roughness)
                                                         2: patch, no ring (curvature)
                                                         3: not supported */
    
    labelModel->setRough(file_models_all, nids_all, cs, 
        params.do_curvature ? params.bad_curvature : params.bad_roughness,
		flags);

    // get input image dimensions
    nlo = file_models[0]->getNL();
    nso = file_models[0]->getNS();

    // Allocate memory for input XYZ and UVW, and output roughness.  The
    // entire images must be in memory for the subroutine.

    for (i=0; i<3; i++) {
	xyz[i] = (double *)malloc(nlo * nso * sizeof(double));
	if (xyz[i] == NULL) {
	    snprintf(msg, msgLen, "Unable to allocate memory for XYZ input %d", i);
	    zvmessage(msg, "");
	    zabend();
	}
        if (uvw_is_matrix) {
	    uvw[i] = (double *)malloc(nlo * nso * sizeof(double));
	    if (uvw[i] == NULL) {
	        snprintf(msg, msgLen, "Unable to allocate memory for UVW input %d", i);
	        zvmessage(msg, "");
	        zabend();
	    }
        }
	out_data[i] = (double *)malloc(nlo * nso * sizeof(double));
	if (out_data[i] == NULL) {
	    snprintf(msg, msgLen, "Unable to allocate memory for output band %d", i);
	    zvmessage(msg, "");
	    zabend();
	}
    }

    // Read in the XYZ file(s)...

    for (band = 0; band < 3; band++) {
        for (line = 0; line < nlo; line++) {
            zvread(xyz_unit[band], (xyz[band]) + (line * nso),
            "BAND", xyz_band[band], "LINE", line+1, NULL);
        }
    }

    if (uvw_is_matrix) {
        // Read in the UVW file(s)...

        for (band = 0; band < 3; band++) {
            for (line = 0; line < nlo; line++) {
                zvread(uvw_unit[band], (uvw[band]) + (line * nso),
               "BAND", uvw_band[band], "LINE", line+1, NULL);   
            }
        }
    } else {
        uvw[0] = &uvw_vector[0];
        uvw[1] = &uvw_vector[1];
        uvw[2] = &uvw_vector[2];
    }

    // Convert coord systems if necessary...

    if (cs != xyz_cs) {
	for (line = 0; line < nlo; line++) {
	    for (int samp = 0; samp < nso; samp++) {
		int index = line * nso + samp;
		PigPoint old_xyz(*(xyz[0]+index), *(xyz[1]+index),
					*(xyz[2]+index));

		// If the point is 0,0,0 (meaning not valid), leave it alone

		if (old_xyz.getX() != 0.0 ||
		    old_xyz.getY() != 0.0 ||
		    old_xyz.getZ() != 0.0) {

		    PigPoint new_xyz = cs->convertPoint(old_xyz, xyz_cs);
		    *(xyz[0]+index) = new_xyz.getX();
		    *(xyz[1]+index) = new_xyz.getY();
		    *(xyz[2]+index) = new_xyz.getZ();
		}
	    }
	}
    }

    if (cs != uvw_cs) {
        if (uvw_is_matrix) {
	    for (line = 0; line < nlo; line++) {
	        for (int samp = 0; samp < nso; samp++) {
		    int index = line * nso + samp;
		    PigVector old_uvw(*(uvw[0]+index), *(uvw[1]+index),
					    *(uvw[2]+index));

		    // If the point is 0,0,0 (meaning not valid), leave it alone

		    if (old_uvw.getX() != 0.0 ||
		        old_uvw.getY() != 0.0 ||
		        old_uvw.getZ() != 0.0) {

		        PigVector new_uvw = cs->convertVector(old_uvw, uvw_cs);
		        *(uvw[0]+index) = new_uvw.getX();
		        *(uvw[1]+index) = new_uvw.getY();
		        *(uvw[2]+index) = new_uvw.getZ();
		    }
	        }
	    }
        } else {
            if (uvw_vector[0] != 0.0 ||
                uvw_vector[1] != 0.0 ||
                uvw_vector[2] != 0.0) {

                PigVector old_uvw(uvw_vector[0], uvw_vector[1], uvw_vector[2]);
                PigVector new_uvw = cs->convertVector(old_uvw, uvw_cs);
                uvw_vector[0] = new_uvw.getX();
                uvw_vector[1] = new_uvw.getY();
                uvw_vector[2] = new_uvw.getZ();
            }
        }
    }

    // Do the work...

    status = xyz_uvw_to_roughness(&params, xyz, uvw, nlo, nso, uvw_is_matrix, out_data, omp_on);

    if (status != 0) {
	snprintf(msg, msgLen, "xyz_uvw_to_roughness failed!! status code=%d", status);
	zvmessage(msg, "");
	zabend();
    }

    // Write out the roughness file(s)...

    for (band = 0; band < num_out_bands; band++) {
	for (line = 0; line < nlo; line++) {
	    zvwrit(out_unit, out_data[band] + (line * nso),
			"BAND", band+1, "LINE", line+1, NULL);
	}
    }

    zvclose(out_unit, NULL);
}

////////////////////////////////////////////////////////////////////////
// Open one set of inputs, either XYZ or UVW...
////////////////////////////////////////////////////////////////////////

void open_inputs(int nids, PigFileModel *file_models[], int unit[3],
		int band[3], char *type)
{
    int i;
    const size_t msgLen = 256;
    char msg[msgLen];

    if (nids == 1) {
      
        // Make sure file is open with U_FORMAT of DOUB to match our buffer.

	// get Unit id
	unit[0] = file_models[0]->getUnit();

        if (file_models[0]->isFileOpen())
	    file_models[0]->closeFile();
	zvopen(unit[0], "op", "read", "open_act", "sa",
		"io_act", "sa", "u_format", "doub", NULL);
	file_models[0]->setFileOpen(TRUE);

	if (file_models[0]->getNB() != 3) {
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
    else if (nids == 3) {
        for (i = 0; i < 3; i++) {

            // make sure that file is open
            if (file_models[i]->isFileOpen())
	        file_models[i]->closeFile();
      
	    // get Unit id
	    unit[i] = file_models[i]->getUnit();

	    zvopen(unit[i], "op", "read", "open_act", "sa",
		"io_act", "sa", "u_format", "doub", NULL);
	    file_models[i]->setFileOpen(TRUE);

	    if (file_models[i]->getNB() != 1) {
		snprintf(msg, msgLen, "A three-file %s must have one band each", type);
		zvmessage(msg, "");
	        zabend();
	    }

	    // check that all files are the same size
	    if ((file_models[i]->getNL() != file_models[0]->getNL()) ||
		(file_models[i]->getNS() != file_models[0]->getNS())) {
	        zvmessage("Input is of different size than Input #1", "");
	        zabend();
	    }
	    band[i] = 1;
        }

    }
    else {
	snprintf(msg, msgLen, "MSLROUGH requires either 1 3-band file or 3 single band files as input for %s", type);
	zvmessage(msg, "");
	zabend();
    }
}

